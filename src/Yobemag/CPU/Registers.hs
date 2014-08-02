module Yobemag.CPU.Registers
    ( CPUState(..)
    , Reg8(..)
    , Reg16(..)
    , RegsState(RegsState)
    , getReg8
    , setReg8
    , getReg16
    , setReg16
    ) where

import Yobemag.Binary

data Reg8 = A | F | B | C | D | E | H | L deriving Show
data Reg16 = AF | BC | DE | HL deriving Show

data CPUState = CPUState
    { regsState :: RegsState
    } deriving Show

data RegsState = RegsState
    { regA :: Byte
    , regF :: Byte
    , regB :: Byte
    , regC :: Byte
    , regD :: Byte
    , regE :: Byte
    , regH :: Byte
    , regL :: Byte
    } deriving Show

getReg8 :: Reg8 -> RegsState -> Byte
getReg8 r = case r of
    A -> regA
    F -> regF
    B -> regB
    C -> regC
    D -> regD
    E -> regE
    H -> regH
    L -> regL

setReg8 :: Reg8 -> Byte -> RegsState -> RegsState
setReg8 r n s = case r of
    A -> s { regA = n }
    F -> s { regF = n }
    B -> s { regB = n }
    C -> s { regC = n }
    D -> s { regD = n }
    E -> s { regE = n }
    H -> s { regH = n }
    L -> s { regL = n }

splitRegs :: (Reg8 -> Reg8 -> a) -> Reg16 -> a
splitRegs f r = case r of
    AF -> f A F
    BC -> f B C
    DE -> f D E
    HL -> f H L

getReg16 :: Reg16 -> RegsState -> Word
getReg16 r s = splitRegs pair r where
    pair x y = toWord (getReg y) (getReg x)
    getReg x = getReg8 x s

setReg16 :: Reg16 -> Word -> RegsState -> RegsState
setReg16 r w s = splitRegs pair r where
    pair hr lr = setReg8 hr hi . setReg8 lr lo $ s
    hi = highByte w
    lo = lowByte w
