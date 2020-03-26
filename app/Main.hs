{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}
import Data.Maybe
import Data.IORef 
import Control.Monad 
import qualified Control.Exception as Exc
import qualified Data.Text as T
import qualified Data.ByteString as B

import Data.Label
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS hiding (round, Op, set)

import HW2
import Drawing
import Expression
import Image
import Settings
import Utils (printErr, printExc, replaceAt)

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

data Slot = Slot String ExprV 

defaultSlot :: Slot
defaultSlot = Slot "" $ Leaf $ Variable "Nothing"

getWH :: Ref SingleWindow -> IO (Int, Int)
getWH window = do
    (Width w) <- getW window
    (Height h) <- getH window
    return (w, h)

modifySlots :: ([Slot] -> [Slot]) -> Ref Choice -> Ref SysMenuBar 
          -> Ref SingleWindow -> IORef [Slot] -> IO ()
modifySlots modifier slotChoice bar window slotsRef = do
    prevExprs <- readIORef slotsRef
    let newExprs = modifier prevExprs
        n = length newExprs
    writeIORef slotsRef newExprs
    setIndexChoices n 
    idx <- getRemoveMenuItemIndex bar
    let rmFlag = if n == 1 then MenuItemInactive else MenuItemNormal
    setMode bar idx $ MenuItemFlags [rmFlag]
    void $ setValue slotChoice $ MenuItemByIndex $ AtIndex $ n - 1
    void $ redraw window
  where
    setIndexChoices :: Int -> IO ()
    setIndexChoices n = do
        when (n < 0) $ error "negative n"
        clear slotChoice
        forM_ [0..n-1] (\x -> add slotChoice (T.pack $ show x) Nothing 
                              dummyMenuItemCb $ MenuItemFlags []) 
 
getCurrentSlot :: IORef [Slot] -> Ref Choice -> IO Slot 
getCurrentSlot slotsRef slotChoice = do
    (AtIndex revIndex) <- getValue slotChoice
    prevSlots <- readIORef slotsRef
    let idx = length prevSlots - 1 - revIndex
    return $ prevSlots !! idx

updateSlot :: (Slot -> Slot) -> IORef [Slot] -> Ref Choice -> Ref SingleWindow -> IO ()
updateSlot updateFunc slotsRef indexChoice window = do
    (AtIndex revIdx) <- getValue indexChoice
    prevExpressions <- readIORef slotsRef
    let idx = length prevExpressions - 1 - revIdx
        newExpression = updateFunc $ prevExpressions !! idx
    writeIORef slotsRef $ replaceAt newExpression idx prevExpressions
    redraw window

tryExprRead :: (String -> ExprV) -> T.Text -> IO ExprV
tryExprRead reader text = Exc.catch 
    (Exc.evaluate $ reader $ T.unpack text)
    (\e -> do printExc "parse" e
              return (Leaf $ Variable "Parse error! Check stderr."))

textCb :: (String -> ExprV) -> IORef [Slot] -> Ref Choice -> Ref SingleWindow 
       -> Ref Input -> IO ()
textCb reader slotsRef indexChoice window field = do
    tline <- getValue field
    newExpression <- tryExprRead reader tline
    updateSlot (\(Slot t _) -> Slot t newExpression) slotsRef indexChoice window

parseCb :: IORef [Slot] -> Ref Choice -> Ref SingleWindow -> Ref Input -> IO ()
parseCb = textCb parse

readCb :: IORef [Slot] -> Ref Choice -> Ref SingleWindow -> Ref Input -> IO ()
readCb = textCb (\x -> read x :: ExprV)

showCb :: IORef [Slot] -> Ref Choice -> IO ()
showCb slotsRef indexChoice = do
    (Slot _ expr) <- getCurrentSlot slotsRef indexChoice
    print expr

setTitleCb :: IORef [Slot] -> Ref Choice -> Ref SingleWindow -> Ref Input -> IO ()
setTitleCb slotsRef choice window field = do
    tline <- getValue field
    updateSlot (\(Slot _ e) -> Slot (T.unpack tline) e) slotsRef choice window

addSlotCb :: Ref Choice -> Ref SysMenuBar -> Ref SingleWindow -> IORef [Slot] -> IO ()
addSlotCb = modifySlots $ (defaultSlot:)

removeSlotCb :: Ref Choice -> Ref SysMenuBar -> Ref SingleWindow -> IORef [Slot] -> IO ()
removeSlotCb = modifySlots tail 

acsCb :: Ref Choice -> Ref SysMenuBar -> Ref SingleWindow -> IORef [Slot] -> IO ()
acsCb indexChoice menuBar window slotsRef = do 
    (Slot _ expr) <- getCurrentSlot slotsRef indexChoice
    let (assigns, mainExpr) = assignCommonSubexprs expr
        mainSlot = Slot "Expr" mainExpr
        assignSlots = map (\(n, e) -> Slot n e) assigns
        slots = mainSlot : (reverse assignSlots)
    modifySlots (\_ -> slots) indexChoice menuBar window slotsRef

pAssignCb :: Ref Input -> Ref Choice -> Ref SysMenuBar -> Ref SingleWindow -> IORef [Slot] 
         -> IO ()
pAssignCb field choice menuBar window slotsRef = do
    ttext <- getValue field
    let lns = T.splitOn "\n" $ T.strip ttext
        varsAndExprs = map (map T.strip . T.splitOn "=") lns
        mbPairs = map splitToPair varsAndExprs
        pairs = catMaybes mbPairs
    when (any isNothing mbPairs)
         (printErr "assignment" "Some unexpected patterns occurred while splitting slots. \
                                \Did you make sure that you have only one '=' per line?")
    slots <- mapM (\(v, txt) -> do e <- tryExprRead parse txt
                                   return $ Slot v e) 
                  pairs 
    modifySlots (\_ -> reverse slots) choice menuBar window slotsRef
  where
    splitToPair :: [T.Text] -> Maybe (String, T.Text) 
    splitToPair [var, exprTxt] = Just $ (T.unpack var, exprTxt)
    splitToPair _ = Nothing

foldNPropCb :: Ref Choice -> Ref SysMenuBar -> Ref SingleWindow -> IORef [Slot] 
            -> IO ()
foldNPropCb = modifySlots (reverse . map tupleToSlot . foldAndPropagateConstants
                           . map slotToTuple . reverse) 
  where
    slotToTuple :: Slot -> (String, ExprV)
    slotToTuple (Slot v e) = (v, e)
    tupleToSlot :: (String, ExprV) -> Slot
    tupleToSlot (v, e) = Slot v e

drawCb :: Ref SingleWindow -> FlOffscreen -> IORef GUIConfig -> IORef [Slot] -> IO ()
drawCb window buffer confRef slotsRef = do
    conf <- readIORef confRef
    slots <- readIORef slotsRef
    (winW, winH) <- getWH window
    drawToBuffer conf slots winW winH
    let orig = toPosition (0, 0)
    flcCopyOffscreen orig (toSize (winW, winH)) buffer orig
  where
    drawToBuffer :: GUIConfig -> [Slot] -> Int -> Int -> IO ()
    drawToBuffer conf slots winW winH = do
        flcBeginOffscreen buffer
        -- clear with a filled rect
        flcSetColor $ get bgColor conf
        flcRectf $ toRectangle (0, 0, winW, winH)
        -- set the styles & draw the trees
        flcSetColor $ get fgColor conf
        flcLineStyle (LineDrawStyle (Just $ get lineStyle conf)
                                    (Just $ get capStyle conf)
                                    Nothing) -- forget join style
                     (Just $ Width $ get lineWidth conf)
                     Nothing -- no need for a dash array
        -- set the font
        flcSetFont (get font conf) (FontSize $ get fontSize conf)
        let numTrees = length slots
            centerXIncr = fromIntegral winW / fromIntegral numTrees
            firstCenterX = centerXIncr / 2.0
            centerY = fromIntegral winH / 2.0
            centerXs = take numTrees $ iterate (+centerXIncr) firstCenterX
        mapM_ (\(slot, x) -> drawSlot slot conf x centerY) 
              $ zip slots $ reverse centerXs
        -- switch to window and copy the buffer before reswitching
        flcEndOffscreen 
    drawSlot :: Slot -> GUIConfig -> Double -> Double -> IO ()
    drawSlot (Slot title expr) conf = drawTreeWithTitle conf title expr

removeMenuItemPath :: T.Text
removeMenuItemPath = "&Drawing/&Remove Slot"

getRemoveMenuItemIndex :: Ref SysMenuBar -> IO AtIndex
getRemoveMenuItemIndex menuBar = do
    (Just idx) <- findIndex menuBar $ MenuItemNameLocator 
                  $ MenuItemName removeMenuItemPath
    return $ AtIndex idx

dummyMenuItemCb :: Maybe (Ref MenuItem -> IO ())
dummyMenuItemCb = Nothing

saveImageCb :: Ref SingleWindow -> FlOffscreen -> Ref NativeFileChooser -> IO ()
saveImageCb window buffer chooser = do
    flcBeginOffscreen buffer 
    (w, h) <- getWH window
    chosen <- showWidget chooser
    case chosen of 
      NativeFileChooserPicked -> do
          mbOutFile <- getFilename chooser
          case mbOutFile of 
            (Just outFileTxt) -> do
                imgBytes <- flcReadImage (toRectangle (0, 0, w, h)) False
                let outFile = T.unpack outFileTxt
                    pngBytes = bytesToPng w h imgBytes
                B.writeFile outFile pngBytes
                putStrLn $ "Saved drawing window to " ++ outFile
            _ -> return ()
      _ -> return ()
    flcEndOffscreen

ui :: IO ()
ui = do
    initialConfig <- initializeSettings
    confRef <- newIORef initialConfig
    slotsRef <- newIORef [defaultSlot]
    conf <- readIORef confRef
    -- define the main window
    configWindow <- createSettingsWindow
    window <- windowNew (toSize (windowWidth conf, windowHeight conf)) 
                        Nothing (Just "HW2 GUI")
    -- define the configuraiton window
    begin window
    -- define the drawing space
    drawingBuffer <- flcCreateOffscreen (toSize (get bufferWidth conf,
                                                 get bufferHeight conf)) 
    drawingWindow <- singleWindowCustom (toSize (get drawingWidth conf,
                                                 get drawingHeight conf))
                                        (Just $ Position (X 0) 
                                                         (Y $ get menuHeight conf))
                                        (Just "Drawing Window")
                                        (Just (\self -> drawCb self drawingBuffer 
                                                               confRef slotsRef))
                                        defaultCustomWidgetFuncs
                                        defaultCustomWindowFuncs
    setColor drawingWindow $ get bgColor conf
    begin drawingWindow
    end drawingWindow
    -- define the buttons
    pack <- packNew (toRectangle (get drawingWidth conf, 
                                  get menuHeight conf, 
                                  get sidebarWidth conf, 
                                  windowHeight conf)) 
                    (Just "MainButtons")
    setBox pack DownFrame
    setResizable window (Just drawingWindow)
    begin pack
    let entryH = get sidebarEntryHeight conf
    setSpacing pack $ get sidebarEntrySpacing conf
    -- add a dummy box for spacing
    void $ boxNew (toRectangle (0, 0, 0, 0)) Nothing
    -- slot choice
    slotChoice <- choiceNew (toRectangle (0, 0, 10, entryH)) (Just "Slot Selection: ")
    setAlign slotChoice alignTopLeft
    void $ add slotChoice "0" Nothing dummyMenuItemCb $ MenuItemFlags []
    void $ setValue slotChoice $ MenuItemByIndex $ AtIndex 0
    -- parsing input field
    textField <- inputNew (toRectangle (0, 0, 0, 2 * entryH)) 
                           (Just "Text input:") (Just FlMultilineInput)
    setAlign textField alignTopLeft
    -- side stuff under the input field
    let sideButtonNew txt = buttonNew (toRectangle (0, 0, 0, entryH)) (Just txt) 
    readButton <- sideButtonNew "Read"
    parseButton <- sideButtonNew "Parse"
    showButton <- sideButtonNew "Show"
    setTitleButton <- sideButtonNew "Set Slot Title"
    pAssignButton <- sideButtonNew "Parse Assignments"
    foldNPropButton <- sideButtonNew "Fold && Propagate Consts"
    acsButton <- sideButtonNew "Assign Common Subexprs"
    reducePolyButton <- sideButtonNew "Reduce Polynomial"
    -- foldButton <- sideButtonNew "Fold Constants"
    end pack
    -- sys menu bar with extra stuff
    menuBar <- sysMenuBarNew (toRectangle (0, 0, windowWidth conf, 
                                                 get menuHeight conf)) 
                             (Just "Menu Bar")
    end window
    -- selector for saving
    chooser <- nativeFileChooserNew $ Just BrowseSaveFile
    setPresetFile chooser "exprs.png"
    setTitle chooser "Save Drawing"
    configFields <- setupSettingsWindow configWindow drawingWindow confRef
    setCallback parseButton $ \_ -> parseCb slotsRef slotChoice drawingWindow textField
    setCallback readButton $ \_ -> readCb slotsRef slotChoice drawingWindow textField
    setCallback showButton $ \_ -> showCb slotsRef slotChoice
    setCallback setTitleButton $ \_ -> setTitleCb slotsRef slotChoice drawingWindow textField
    setCallback acsButton $ \_ -> acsCb slotChoice menuBar drawingWindow slotsRef
    setCallback pAssignButton $ \_ -> 
        pAssignCb textField slotChoice menuBar drawingWindow slotsRef
    setCallback foldNPropButton $ \_ -> 
        foldNPropCb slotChoice menuBar drawingWindow slotsRef
    let transformCb f = \_ -> updateSlot (\(Slot t e) -> Slot t $ f e) 
                                         slotsRef slotChoice drawingWindow
    setCallback reducePolyButton $ transformCb reducePoly
    -- setCallback foldButton $ transformCb foldConstants
    let addSysMenuItem entry flgs cb = 
            void $ addAndGetMenuItem menuBar entry Nothing 
                                     (wrapMenuItemCb cb)
                                     $ MenuItemFlags flgs
    addSysMenuItem "&Drawing/&Save As" [MenuItemNormal]
                $ saveImageCb drawingWindow drawingBuffer chooser
    addSysMenuItem "&Drawing/&Add Slot" [MenuItemNormal]  
        $ addSlotCb slotChoice menuBar drawingWindow slotsRef
    addSysMenuItem removeMenuItemPath [MenuItemInactive] 
        $ removeSlotCb slotChoice menuBar drawingWindow slotsRef
    addSysMenuItem "&Drawing/&Configure" [MenuItemNormal] 
        $ configCb configWindow configFields confRef
    showWidget window
  where
    wrapMenuItemCb :: IO () -> Maybe (Ref MenuItem -> IO ())
    wrapMenuItemCb cb = Just $ \_ -> cb

main :: IO ()
main = ui >> FL.run >> FL.flush
