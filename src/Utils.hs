module Utils where 

import Control.Exception (SomeException)
import Data.List (foldl')
import qualified System.IO as IO

toDouble :: Integral a => a -> Double
toDouble x = fromIntegral x :: Double

mean :: (Foldable t, Real a, Fractional b) => t a -> b
mean xs = let (total, len) = foldl' (\(t, l) x -> (t+x, l+1)) (0, 0 :: Int) xs
           in realToFrac total / realToFrac len 

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = let (group, rest) = splitAt n xs
               in group : groupN n rest

rlookup :: Eq b => b -> [(a, b)] -> Maybe a
rlookup ritem pairs = lookup ritem $ map (\(x, y) -> (y, x)) pairs

printErr :: String -> String -> IO ()
printErr etype errStr = do
    let ePutStrLnEnd = if last errStr == '\n'
                          then ePutStr
                          else ePutStrLn
    ePutStr $ etype ++ " error: \27[1;31m" -- red & bold escape 
    ePutStr errStr -- SomeException is the catch-all exc type
    ePutStrLnEnd "\27[0m" -- restore color
    eFlush -- flush color restoration in case there was a newline before
  where
    ePutStr = IO.hPutStr IO.stderr
    ePutStrLn = IO.hPutStrLn IO.stderr
    eFlush = IO.hFlush IO.stderr

printExc :: String -> SomeException -> IO ()
printExc etype e = printErr etype $ show e

-- this is something really bad to do with lists, but I
-- want to do it since I will only use it on really small
-- lists, and it is more practical than importing
-- yet another data structure just for this purpose
replaceAt :: a -> Int -> [a] -> [a]
replaceAt y k xs = let (left, _:right) = splitAt k xs
                    in left ++ (y : right)
