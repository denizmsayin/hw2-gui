module Image where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L 

import Codec.Picture.Types
import Codec.Picture.Saving

import Utils (groupN)

bytesToImage :: Int -> Int -> B.ByteString -> DynamicImage
bytesToImage w h rawData = do 
    let byteGroups = groupN 3 $ B.unpack rawData
        (_, img) = generateFoldImage (\([r,g,b]:rest) _ _ -> (rest, PixelRGB8 r g b))
                                     byteGroups w h
     in ImageRGB8 img

bytesToPng :: Int -> Int -> B.ByteString -> B.ByteString
bytesToPng w h bytes = L.toStrict $ imageToPng $ bytesToImage w h bytes
