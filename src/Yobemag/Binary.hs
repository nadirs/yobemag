module Yobemag.Binary where

import qualified Data.Word as W
import Data.Bits


type Word = W.Word16
type Byte = W.Word8
type Flag = Bool

msb :: Word -> Byte
msb = fromIntegral . (`shiftR` 8)

lsb :: Word -> Byte
lsb = fromIntegral . (.&. 0xff)

-- arguments in little endian order
toWord :: Byte -> Byte -> Word
toWord l h = shiftL (fromIntegral h) 8 + fromIntegral l
