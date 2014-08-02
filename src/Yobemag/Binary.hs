module Yobemag.Binary
    ( Word
    , Byte
    , highByte
    , lowByte
    , toWord
    ) where

import qualified Data.Word as W
import Data.Bits


type Word = W.Word16
type Byte = W.Word8

highByte :: Word -> Byte
highByte = fromIntegral . (`shiftR` 8)

lowByte :: Word -> Byte
lowByte = fromIntegral . (.&. 0xff)

-- arguments in little endian order
toWord :: Byte -> Byte -> Word
toWord l h = shiftL (fromIntegral h) 8 + fromIntegral l
