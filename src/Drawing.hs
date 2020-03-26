{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}
module Drawing where

import Geom2D
import Expression
import Settings (GUIConfig, leafSpacing, levelSpacing, branchScaling)
import Utils (mean, toDouble)

import Data.Text (pack)
import Control.Monad

import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS hiding (round, Op, set)

import Data.Label

-- The font height business seems pretty weird! I thought doing + h / 2
-- would be enough and that drawing starting from the bottom left was the
-- best approach, but that is not exactly the case; letters like g and y
-- go below the bottom left, and thus I somehow need to account for them
-- to make full use of the font height. I'll assume it is 25% of the height,
-- and thus only add h/4 rather than h/2. 
-- Experiments show that this idea seems to be working pretty nicely.
drawCenteredStr :: String -> Double -> Double -> IO CenterBox
drawCenteredStr s x y = do
    let text = pack s
    (Height h) <- flcHeight
    (PreciseWidth dblW) <- flcWidth text
    let dblH = toDouble h 
        lx = round $ x - dblW / 2
        ly = round $ y + dblH / 4
    flcDraw text $ Position (X lx) (Y ly)
    return $ CenterBox dblW dblH $ Point x y

-- draws boxes, useful for debugging
drawCenterBox :: CenterBox -> IO ()
drawCenterBox (CenterBox w1 h1 (Point x1 y1)) = do
    flcRect (toRectangle (round (x1 - w1/2), round (y1 - h1/2), 
                          round w1, round h1))

-- Connecting boxes together requires a lot of geometry as we do not
-- want lines to be drawn inside the boxes, but only between their
-- boundaries. Line equations and stuff, here we go!

drawBoxConnections :: GUIConfig -> CenterBox -> CenterBox -> IO ()
drawBoxConnections conf b1@(CenterBox _ _ c1) 
                    b2@(CenterBox _ _ c2) = do
    let connection = Segment c1 c2
        boxP1 = intersectBoxSegment b1 connection
        boxP2 = intersectBoxSegment b2 connection
        boundaryConn = Segment boxP1 boxP2
        scaledConn = scaleSegment boundaryConn $ get branchScaling conf
        (Segment (Point dX1 dY1) (Point dX2 dY2)) = scaledConn
    flcBeginLine
    flcVertex $ PrecisePosition (PreciseX dX1) (PreciseY dY1)
    flcVertex $ PrecisePosition (PreciseX dX2) (PreciseY dY2)
    flcEndLine

drawTreeWithTitle :: Repr a => GUIConfig-> String -> Expression a -> Double -> Double
                  -> IO ()
drawTreeWithTitle conf title expr centerX centerY = do
    (CenterBox _ h (Point _ ry)) <- drawTree conf expr centerX centerY
    void $ drawCenteredStr title centerX $ ry - h

drawTree :: Repr a => GUIConfig -> Expression a -> Double -> Double -> IO CenterBox 
drawTree conf exprTree treeCenterX treeCenterY = do
    (PreciseWidth w) <- exprWidth exprTree
    (Height ih) <- exprHeight exprTree
    (Height iFontH) <- flcHeight
    let h = fromIntegral ih
        fontH = fromIntegral iFontH
        startLeafX = treeCenterX - w / 2.0
        startCenterY = treeCenterY + (-h + fontH) / 2.0
    (_, rootBox) <- drawExpression exprTree startLeafX startCenterY
    return rootBox
  where
    exprWidth :: Repr a => Expression a -> IO PreciseWidth
    exprWidth expr = do
        let spacing = fromIntegral $ (countLeaves expr - 1) * (get leafSpacing conf)
        (PreciseWidth textWidth) <- leafWidth expr
        return $ PreciseWidth $ spacing + textWidth
    countLeaves :: Expression a -> Int
    countLeaves (Leaf _) = 1
    countLeaves (UnaryOperation _ e) = countLeaves e
    countLeaves (BinaryOperation _ e1 e2) = countLeaves e1 + countLeaves e2
    leafWidth :: Repr a => Expression a -> IO PreciseWidth
    leafWidth (Leaf sth) = flcWidth $ pack $ repr sth
    leafWidth (UnaryOperation _ expr) = leafWidth expr
    leafWidth (BinaryOperation _ e1 e2) = do
        (PreciseWidth w1) <- leafWidth e1 
        (PreciseWidth w2) <- leafWidth e2
        return $ PreciseWidth $ w1 + w2

    exprHeight :: Expression a -> IO Height
    exprHeight expr = do
        (Height h) <- flcHeight
        return $ Height $ h + exprDepth expr * (get levelSpacing conf)
    exprDepth (Leaf _) = 0
    exprDepth (UnaryOperation _ e) = 1 + exprDepth e
    exprDepth (BinaryOperation _ e1 e2) = 1 + max (exprDepth e1) (exprDepth e2)
    -- how does drawExpression work? Take a the height and leftmost leaf position
    -- to draw strings at, return a new leaf position and the rectangle where
    -- the lower node has been drawn.
    drawExpression :: Repr a => Expression a -> Double -> Double -> IO (Double, CenterBox) 
    drawExpression (Leaf l) leafX centerY = drawLeaf (repr l) leafX centerY
    drawExpression (UnaryOperation op expr) leafX centerY = 
        drawIntermediate (repr op) [expr] leafX centerY
    drawExpression (BinaryOperation op leftExpression rightExpression) leafX centerY = 
        drawIntermediate (repr op) [leftExpression, rightExpression] leafX centerY

    drawLeaf :: String -> Double -> Double -> IO (Double, CenterBox)
    drawLeaf string leafX centerY = do
        (PreciseWidth sw) <- flcWidth $ pack string
        let centerX = leafX + sw / 2
        strBox <- drawCenteredStr string centerX centerY
        let newLeafX = leafX + sw + fromIntegral (get leafSpacing conf)
        return (newLeafX, strBox) 

    drawIntermediate :: Repr a => String -> [Expression a] -> Double -> Double 
                     -> IO (Double, CenterBox)
    drawIntermediate nodeStr exprs leafX centerY = do
        let lowerY = nextLevel centerY
        -- monadic left fold to draw all subtrees while accumulating boxes
        (lastLeafX, allBoxes) <- foldM (\(leafX', boxes) expr -> do 
                                            (nextLeafX, box) <- 
                                                drawExpression expr leafX' lowerY
                                            return (nextLeafX, box:boxes))
                                       (leafX, []) 
                                       exprs
        let centerX = mean $ map (\(CenterBox _ _ (Point x _)) -> x) allBoxes
        strBox <- drawCenteredStr nodeStr centerX centerY
        -- monadic map to connect all boxes
        mapM_ (drawBoxConnections conf strBox) allBoxes
        return (lastLeafX, strBox)

    nextLevel :: Double -> Double
    nextLevel y = y + fromIntegral (get levelSpacing conf)

