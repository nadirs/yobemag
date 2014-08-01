module Yobemag.CPU.Registers where

import Data.List (intercalate)
import Yobemag.Binary

{--
 -- Flags:
 -- z  → zero
 -- c  → carry
 -- h → half-carry
 -- n → negative
 --}
type Zero = Bool
type Carry = Bool
type HalfCarry = Bool
type Neg = Bool
newtype Flags = Flags
              { getFlags :: (Zero, Carry, HalfCarry, Neg)
              } deriving (Eq)

instance Show Flags where
    show (Flags (z, c, h, n)) = intercalate " | " . map (uncurry showf) $ [(z, "Z"), (c, "C"), (h, "H"), (n, "N")]
      where
        showf True s = s
        showf False s = 'N' : s

data Registers = Registers
               { a :: Byte
               , f :: Flags
               , bc :: Word
               , de :: Word
               , hl :: Word
               , sp :: Word
               } deriving (Show, Eq)

emptyRegisters :: Registers
emptyRegisters = Registers
              { a = 0
              , f = emptyFlags
              , bc = 0
              , de = 0
              , hl = 0
              , sp = 0
              }

emptyFlags :: Flags
emptyFlags = Flags (False, False, False, False)
