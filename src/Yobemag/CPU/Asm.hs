module Yobemag.CPU.Asm where

import Yobemag.Binary
import Yobemag.CPU.Registers


ldReg8Byte :: Reg8 -> Byte -> CPUState -> CPUState
ldReg8Byte r b s = s { regsState = setReg8 r b (regsState s) }

ldReg8Reg8 :: Reg8 -> Reg8 -> CPUState -> CPUState
ldReg8Reg8 ro ri s = s { regsState = setReg8 ro b regs } where
    regs = regsState s
    b = getReg8 ri regs

ldReg16Word :: Reg16 -> Word -> CPUState -> CPUState
ldReg16Word r w s = s { regsState = setReg16 r w (regsState s) }

ldReg16Reg16 :: Reg16 -> Reg16 -> CPUState -> CPUState
ldReg16Reg16 ro ri s = s { regsState = setReg16 ro w regs } where
    regs = regsState s
    w = getReg16 ri regs
