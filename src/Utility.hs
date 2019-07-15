module Utility
(splitAts
,myDecodeInt16
,myDecodeInt32
) where

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy       as BSL
import           Data.Int                   (Int64)

-- Plural version of ByteString.splitAt.
-- If xs runs out early, partial or empty byestrings are returned.
splitAts :: [Int] -> BSLC.ByteString  -> [BSLC.ByteString]
splitAts [] xs = [xs]
splitAts (n:ns) xs = taken:(splitAts ns dropped)
    where (taken, dropped) = BSLC.splitAt (fromIntegral n::Int64) xs  

-- Treat a 2-byte ByteString as an encoded integer.
myDecodeInt16 :: BSLC.ByteString -> Int
myDecodeInt16 bs = if BSLC.length bs == 2 
                    then myDecodeInt' 0 bs 
                    else error "myDecodeInt16: Not 2 bytes"
-- Treat a 4-byte ByteString as an encoded integer.
myDecodeInt32 :: BSLC.ByteString -> Int
myDecodeInt32 bs = if BSLC.length bs == 4 
                    then myDecodeInt' 0 bs 
                    else error "myDecodeInt32: Not 4 bytes"
-- This reads the entire ByteString, so it's crucial to check the
-- number of bytes before calling.
myDecodeInt' :: Int -> BSLC.ByteString -> Int
myDecodeInt' total bs
    | BSLC.null bs = total
    | otherwise = myDecodeInt' ((total * 256) + (fromIntegral (BSL.head bs)::Int)) (BSLC.tail bs)
