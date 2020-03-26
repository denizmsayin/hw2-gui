{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
module Settings where

import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.IORef 
import Control.Monad
import Control.Exception (catch, evaluate)
import Foreign.C.Types (CInt(..))
import qualified Data.List as L 
import qualified Data.Text as T 
import qualified System.IO as IO

import Data.Label
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS hiding (round, Op, set)

import Utils (groupN, rlookup, printExc, toDouble)

-- TODO: add more colors & fonts

-- how to add a new setting? 
-- -> add field in GUIConfig
-- -> add default value in defaultConfig
-- -> add attribute in settingInputAttrs
-- done!

data GUIConfig = GUIConfig {
    _drawingWidth :: Int,
    _drawingHeight :: Int,
    _bufferWidth :: Int,
    _bufferHeight :: Int,
    _sidebarWidth :: Int,
    _sidebarEntryHeight :: Int,
    _sidebarEntrySpacing :: Int,
    _menuHeight :: Int,
    _leafSpacing :: Int,
    _levelSpacing :: Int,
    _bgColor :: Color,
    _fgColor :: Color,
    _font :: Font,
    _fontSize :: CInt,
    _branchScaling :: Double,
    _lineWidth :: Int,
    _lineStyle :: LineStyle,
    _capStyle :: CapStyle
    }
   
mkLabel ''GUIConfig

windowWidth :: GUIConfig -> Int
windowWidth conf = get drawingWidth conf + get sidebarWidth conf

windowHeight :: GUIConfig -> Int
windowHeight conf = get drawingHeight conf + get menuHeight conf

-- properties of config fields:
-- a name, a type of input and a function Lens -> T.Text -> GUIConfig -> GUIConfig

data SettingInpType = FieldInpType FlInputType
                    | ChoiceInpType [T.Text]

data SettingAttribs = SAttribs { attrName :: T.Text,
                                 attrInp :: SettingInpType,
                                 attrGetter :: (GUIConfig -> T.Text),
                                 attrSetter :: (T.Text -> GUIConfig -> GUIConfig)
                               }

colorChoices :: [(T.Text, Color)]
colorChoices = [("Black", blackColor), ("Blue", blueColor), ("Cyan", cyanColor),
                ("Green", greenColor), ("Magenta", magentaColor), ("Red", redColor),
                ("White", whiteColor), ("Yellow", yellowColor), 
                ("Dark Blue", darkBlueColor), ("Dark Cyan",  darkCyanColor),
                ("Dark Green", darkGreenColor), ("Dark Magenta", darkMagentaColor),
                ("Dark Red", darkRedColor), ("Dark Yellow", darkYellowColor)]

fontChoices :: [(T.Text, Font)]
fontChoices = [("Helvetica", helvetica), ("Helvetica - Bold", helveticaBold),
               ("Times New Roman", times), ("Times New Roman - Bold", timesBold)]

lineStyleChoices :: [(T.Text, LineStyle)]
lineStyleChoices = [("Solid", LineStyleSolid), ("Dashed", LineStyleDash), 
                    ("Dash & Dot", LineStyleDashDot), 
                    ("Dash & Dot Dot", LineStyleDashDotDot)]

capStyleChoices :: [(T.Text, CapStyle)]
capStyleChoices = [("Flat", CapStyleFlat), ("Round", CapStyleRound), ("Square", CapStyleSquare)]

settingInputAttrs :: [SettingAttribs]
settingInputAttrs = [
    intAttrib "Window Width" drawingWidth,
    intAttrib "Window Height" drawingHeight,
    intAttrib "Leaf Spacing" leafSpacing,
    intAttrib "Level Spacing" levelSpacing,
    doubleAttrib "Branch Scaling" branchScaling,
    choiceAttrib "Background" bgColor colorChoices, 
    choiceAttrib "Foreground" fgColor colorChoices, 
    choiceAttrib "Font" font fontChoices,
    cIntAttrib "Font Size" fontSize,
    intAttrib "Line Width" lineWidth,
    choiceAttrib "Line Style" lineStyle lineStyleChoices,
    choiceAttrib "Cap Style" capStyle capStyleChoices
    ]
  where
    getr :: Show a => (GUIConfig :-> a) -> (GUIConfig -> T.Text)
    getr = \lensf -> T.pack . show . get lensf

    setr :: forall a. Read a => (GUIConfig :-> a) -> T.Text -> GUIConfig -> GUIConfig
    setr = \lensf txt conf -> set lensf ((read :: String -> a) $ T.unpack txt) conf

    fieldRSAttrib :: (Read a, Show a) => (Proxy a) -> SettingInpType
                  -> (T.Text -> (GUIConfig :-> a) -> SettingAttribs)
    fieldRSAttrib _ inpType = \name lensf -> 
        SAttribs name inpType (getr lensf) (setr lensf)

    intAttrib = fieldRSAttrib (Proxy :: Proxy Int) (FieldInpType FlIntInput)
    doubleAttrib = fieldRSAttrib (Proxy :: Proxy Double) (FieldInpType FlFloatInput)
    cIntAttrib = fieldRSAttrib (Proxy :: Proxy CInt) (FieldInpType FlIntInput)

    choiceAttrib :: Eq a => T.Text -> (GUIConfig :-> a) -> [(T.Text, a)] -> SettingAttribs
    choiceAttrib name lensf pairs = 
        SAttribs { attrName = name,
                   attrInp = ChoiceInpType $ map fst pairs,
                   attrGetter = \conf -> fromJust $ rlookup (get lensf conf) pairs,
                   attrSetter = \txt conf -> set lensf (fromJust $ lookup txt pairs) conf
                 }



-- I'm not deriving Show since I have a bit of a bad design issue,
-- the setting attributes should be grouped together with the
-- config itself as a single whole entity. Instead, I have a mutable
-- config and immutable attributes which are decoupled.
showSettings :: GUIConfig -> T.Text
showSettings conf = 
    let lns = zipWith (\name txt -> name `T.append` " = " `T.append` txt)
                      (map attrName settingInputAttrs)
                      (map (\g -> g conf) (map attrGetter settingInputAttrs))
     in T.intercalate "\n" lns

readSettings :: T.Text -> GUIConfig
readSettings repr = 
    let lns = T.splitOn "\n" $ T.strip repr
        kvs = map (\ln -> map T.strip $ T.splitOn "=" ln) lns
        readConf = L.foldl' (\conf [k, v] -> let (Just attr) = L.find (\a -> attrName a == k)
                                                                      settingInputAttrs
                                              in (attrSetter attr) v conf)
                            defaultConfig kvs
     in readConf 

defaultConfig :: GUIConfig
defaultConfig = GUIConfig { _drawingWidth = 1200,
                            _drawingHeight = 500,
                            _bufferWidth = 4096,
                            _bufferHeight = 2160,
                            _sidebarWidth = 200,
                            _sidebarEntryHeight = 30,
                            _sidebarEntrySpacing = 16,
                            _menuHeight = 25,
                            _leafSpacing = 20,
                            _levelSpacing = 40,
                            _bgColor = whiteColor,
                            _fgColor = blackColor,
                            _font = helvetica,
                            _fontSize = 18,
                            _branchScaling = 0.9,
                            _lineWidth = 0,
                            _lineStyle = LineStyleSolid,
                            _capStyle = CapStyleFlat
                          }

-- no label generation for this one since it will
-- never be modified in the context of the program. 
 

data SettingsConstants = SettingsConstants {
    _settingsWidth :: Int,
    _settingsHeight :: Int,
    _vertSpacing :: Int,
    _inputsPerCol :: Int,
    _settingsFont :: Font,
    _settingsFSize :: CInt,
    _settingsFieldRatio :: Double,
    _settingsButtonHeight :: Int, 
    _settingsButtonWidth :: Int,
    _settingsButtonMarginMult :: Double,
    _settingsFieldHeight :: Int,
    _settingsFile :: String 
    } deriving Show

mkLabel ''SettingsConstants

settingsConfig :: SettingsConstants
settingsConfig = SettingsConstants { _settingsWidth = 800,
                                     _settingsHeight = 200,
                                     _vertSpacing = 20,
                                     _inputsPerCol = 3,
                                     _settingsFont = times,
                                     _settingsFSize = 12,
                                     _settingsFieldRatio = 0.48,
                                     _settingsButtonWidth = 60,
                                     _settingsButtonHeight = 30,
                                     _settingsButtonMarginMult = 1.2,
                                     _settingsFieldHeight = 30,
                                     _settingsFile = ".hw2_gui_settings"
                                   }

-- what's the goal? Automatically deriving a config window
-- from the GUIConfig fields. What's needed?

data SettingInp = FieldInp (Ref Input)
                | ChoiceInp (Ref Choice)

configCb :: Ref Window -> [SettingInp] -> IORef GUIConfig -> IO ()
configCb configWindow configFields confRef = do
    conf <- readIORef confRef
    let getters = map attrGetter settingInputAttrs 
    zipWithM_ (\inp getter -> setInp inp $ getter conf) configFields getters
    showWidget configWindow
  where
    setInp :: SettingInp -> T.Text -> IO ()
    setInp (FieldInp inp) txt = void $ setValue inp txt
    setInp (ChoiceInp inp) txt = do
        (Just idx) <- findIndex inp $ MenuItemNameLocator $ MenuItemName txt
        void $ setValue inp $ MenuItemByIndex $ AtIndex idx
--
initializeSettings :: IO GUIConfig
initializeSettings = do
    catch (do let inFile = get settingsFile settingsConfig
              h <- IO.openFile inFile IO.ReadMode
              conts <- IO.hGetContents h -- lazy I/O :/
              settings <- evaluate $ readSettings $ T.pack conts
              IO.hClose h
              putStrLn $ "Successfully read config from: " ++ inFile
              return settings)
          (\e -> do putStrLn "Config file does not exist or corrupted, using defaults. See error: "
                    printExc "config read" e
                    return defaultConfig)

createSettingsWindow :: IO (Ref Window)
createSettingsWindow = windowNew (toSize (get settingsWidth settingsConfig, 
                                         get settingsHeight settingsConfig)) 
                                 Nothing (Just "Configuration")

setupSettingsWindow :: Ref Window -> Ref SingleWindow -> IORef GUIConfig -> IO [SettingInp]
setupSettingsWindow configWindow drawingWindow guiConfigRef = do
    let conf = settingsConfig
        winW = get settingsWidth conf
        winH = get settingsHeight conf
        numFields = length settingInputAttrs
        fPC = get inputsPerCol settingsConfig
        numCols = ceiling $ toDouble numFields / toDouble fPC 
        -- colWidth = winW `quot` numCols
        -- weirdly, there are no methods to get text extents
        -- outside of a drawing environment, thus I cannot align
        -- labels exactly... Well, time for more heuristics from the config!
        colWidth = winW `quot` numCols
        packWidth = round $ toDouble colWidth * (get settingsFieldRatio settingsConfig)
        labelWidth = colWidth - packWidth
        buttonH = get settingsButtonHeight settingsConfig
        bMarginMult = get settingsButtonMarginMult settingsConfig
        buttonSpaceH = round $ toDouble buttonH * bMarginMult  
        packHeight = winH - buttonSpaceH
    begin configWindow
    -- split the inputs into columns and place them
    let fieldCols = groupN (get inputsPerCol settingsConfig) settingInputAttrs
    (_, inputs) <- foldM (\(x, cols) bGroup -> 
                              do col <- makeCol (x + labelWidth) 
                                               packWidth packHeight bGroup
                                 return (x + colWidth, cols ++ col)) 
                         (0, []) fieldCols
    -- add apply & cancel buttons
    let singleMarginMult = 1.0 + (bMarginMult - 1.0) / 2.0 
        buttonW = get settingsButtonWidth settingsConfig
        buttonStartX = winW - round (singleMarginMult * toDouble buttonW)
        buttonY = winH - round (singleMarginMult * toDouble buttonH)
        buttonSpaceX = round $ toDouble buttonW * bMarginMult
    (_, [persistButton, applyButton, cancelButton]) <- 
        foldM (\(x, buttons) name -> do button <- makeLRButton x buttonY name
                                        return (x - buttonSpaceX, button:buttons))
              (buttonStartX, []) ["Cancel" , "Apply", "Persist"]
    setCallback cancelButton (\_ -> hide configWindow)
    setCallback applyButton (\_ -> applyCb inputs)
    setCallback persistButton (\_ -> persistCb inputs)
    end configWindow 
    return inputs
  where
    persistCb :: [SettingInp] -> IO ()
    persistCb inputs = do
        applyCb inputs -- apply the settings first
        conf <- readIORef guiConfigRef
        let confStr = T.unpack $ showSettings conf
            outFile = get settingsFile settingsConfig
        catch (IO.withFile outFile IO.WriteMode
                           (\h -> IO.hPutStrLn h confStr))
              (\e -> printExc "save" e)
        putStrLn $ "Settings saved to: '" ++ outFile ++ "', delete it to restore defaults."

    applyCb :: [SettingInp] -> IO ()
    applyCb inputs = do
        values <- mapM getSettingValue inputs
        curConf <- readIORef guiConfigRef
        let setters = map attrSetter settingInputAttrs
            newConf = foldl (\conf (value, setter) -> setter value conf)
                            curConf (zip values setters)
        writeIORef guiConfigRef newConf
        redraw drawingWindow
      where
        getSettingValue :: SettingInp -> IO T.Text
        getSettingValue (FieldInp field) = getValue field
        getSettingValue (ChoiceInp choice) = getText choice 

    makeLRButton :: Int -> Int -> T.Text -> IO (Ref Button)
    makeLRButton x y txt = do
        buttonNew (toRectangle (x, y, get settingsButtonWidth settingsConfig,
                                      get settingsButtonHeight settingsConfig))
                  (Just txt)

    makeCol :: Int -> Int -> Int -> [SettingAttribs] -> IO [SettingInp] 
    makeCol packX packW packH attribs = do
        col <- packNew (toRectangle (packX, 0, packW, packH))
                       Nothing
        setSpacing col $ get vertSpacing settingsConfig
        begin col
        inputs <- mapM makeInp attribs
        end col
        return inputs

    makeInp :: SettingAttribs -> IO SettingInp
    makeInp (SAttribs sName (FieldInpType inpType) _ _) = do
        inp <- inputNew inpRect (inpLabel sName) (Just inpType)
        setLabelfont inp $ get settingsFont settingsConfig
        setLabelsize inp $ FontSize $ get settingsFSize settingsConfig
        return $ FieldInp inp
    makeInp (SAttribs sName (ChoiceInpType choices) _ _) = do
        inp <- choiceNew inpRect $ inpLabel sName 
        mapM_ (\x -> add inp x Nothing dummyChoiceCb $ MenuItemFlags []) choices
        setLabelfont inp $ get settingsFont settingsConfig
        setLabelsize inp $ FontSize $ get settingsFSize settingsConfig
        return $ ChoiceInp inp

    dummyChoiceCb :: Maybe ((Ref MenuItemBase) -> IO ())
    dummyChoiceCb = Nothing

    inpRect = toRectangle (0, 0, 0, get settingsFieldHeight settingsConfig)
    inpLabel :: T.Text -> Maybe T.Text
    inpLabel name = Just $ T.append name ":"
