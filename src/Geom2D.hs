module Geom2D where

import Data.Maybe

-- a bit of geometry is required for drawing
-- box connections, since we do not want
-- lines to be drawn inside the text boxes, but only
-- between their boundaries; which leads to line
-- segment intersections & stuff

-- You will be writing a lot of code like this
-- during your graphics course (CENG477)!

-- mx + n = y form, m & n are stored
data Point = Point Double Double deriving Show

-- can't implement cross-class overloading
-- without GHC extensions, like everything...
class LineLike a where
    intersection :: a -> a -> Maybe Point

data Line = Vertical Double -- x = a
          | Sloped Double Double -- y = mx + n
          deriving Show

line :: Point -> Point -> Line
line (Point x1 y1) (Point x2 y2) = 
    if x1 == x2
       then Vertical x1 
       else let m = (y2 - y1) / (x2 - x1)
                n = y1 - m * x1
             in Sloped m n

instance LineLike Line where
    intersection (Vertical x) (Sloped m n) = Just $ Point x (m*x + n)
    intersection s@(Sloped _ _) v@(Vertical _) = intersection v s
    intersection (Vertical x1) (Vertical x2) = 
        if x1 == x2
           then error "Lines are on top of each other!"
           else Nothing
    intersection (Sloped m1 n1) (Sloped m2 n2) =
        if m1 == m2
           then if n1 == n2
                   then error "Lines are on top of each other!"
                   else Nothing
           else let x = (n2 - n1) / (m1 - m2)
                    y = m1 * x + n1
                 in Just $ Point x y

data Segment = Segment Point Point deriving Show

segment :: Point -> Point -> Segment
segment p1 p2 = Segment p1 p2

scaleSegment :: Segment -> Double -> Segment
scaleSegment (Segment (Point x1 y1) (Point x2 y2)) factor = 
    let cx = (x1 + x2) / 2
        sx1 = cx + factor * (cx - x1)
        sx2 = cx + factor * (cx - x2)
        cy = (y1 + y2) / 2
        sy1 = cy + factor * (cy - y1)
        sy2 = cy + factor * (cy - y2)
     in Segment (Point sx1 sy1) (Point sx2 sy2)

instance LineLike Segment where
    intersection s1@(Segment p11 p12) s2@(Segment p21 p22) =
        let lineIntr = intersection (line p11 p12) (line p21 p22)
         in case lineIntr of Nothing -> Nothing
                             Just intr -> if inBox s1 intr && inBox s2 intr
                                             then Just intr
                                             else Nothing 
      where
        inBox (Segment (Point x1 y1) (Point x2 y2)) (Point x y) =
            inRange x x1 x2 && inRange y y1 y2
        inRange x a b = (a - tol <= x && x <= b + tol)
                        || (b - tol <= x && x <= a + tol)
        tol = 1e-7

data CenterBox = CenterBox Double Double Point deriving Show -- width, height, center 

-- only considering the safe case with one exact intersection
-- as when connecting boxes in our GUI, that's refreshing!
intersectBoxSegment :: CenterBox -> Segment -> Point
intersectBoxSegment (CenterBox w h (Point x y)) seg =
    let corners = [Point (x + w/2) (y + h/2), Point (x + w/2) (y - h/2),
                   Point (x - w/2) (y - h/2), Point (x - w/2) (y + h/2)]
        mbIntersections = map (intersection seg) $ segSequence corners
        intersections = filter isJust mbIntersections
     in fromJust $ head intersections -- assume one intersection exists
     -- multiple intersections arise if the line falls near a corner, just take the first
  where
    segSequence :: [Point] -> [Segment]
    segSequence points = zipWith Segment points (tail $ cycle points)
