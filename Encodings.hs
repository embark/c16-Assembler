{-# LANGUAGE ViewPatterns #-}

module Encodings where

--import Data.List
--import Data.Int
--import Data.Bits
--import Data.Char
--import Data.Maybe
--import Data.Either
--import Control.Applicative
--import Control.Monad
--import Numeric


type Reg = String
type Imm5 = String
type Imm8 = String
type Imm16 = String

data Format = A Reg Reg Reg
            | B Reg Reg Imm5
            | C Reg Imm8
            | E
            | F Imm8
            | G Reg Imm5
            | H Reg Reg
            | I Imm16
            | J Reg
    deriving (Show)

type MachineCode = String
type AssemblyCode = String
type Instruction = String
type Error = String


encode :: Instruction -> Format -> Either Error MachineCode

-- Normal Instructions --
encode "add"  (B rd ra i)  = Right $ "00000" ++ rd ++ ra ++ i
encode "add"  (A rd ra rb) = Right $ "00001" ++ rd ++ ra ++ "00" ++ rb
encode "slt"  (B rd ra i)  = Right $ "00100" ++ rd ++ ra ++ i
encode "slt"  (A rd ra rb) = Right $ "00101" ++ rd ++ ra ++ "00" ++ rb
encode "shl"  (B rd ra i)  = Right $ "10000" ++ rd ++ ra ++ i
encode "shl"  (A rd ra rb) = Right $ "10001" ++ rd ++ ra ++ "00" ++ rb
encode "ld"   (B rd ra i)  = Right $ "10100" ++ rd ++ ra ++ i
encode "ld"   (C rd i)     = Right $ "10101" ++ rd ++ i
encode "st"   (B rd ra i)  = Right $ "10110" ++ rd ++ ra ++ i
encode "st"   (C rd i)     = Right $ "10111" ++ rd ++ i
encode "lea"  (B rd ra i)  = Right $ "11000" ++ rd ++ ra ++ i
encode "lea"  (C rd i)     = Right $ "11001" ++ rd ++ i
encode "call" (B rd ra i)  = Right $ "11010" ++ rd ++ ra ++ i
encode "call" (C rd i)     = Right $ "11011" ++ rd ++ i
encode "brz"  (B rd ra i)  = Right $ "11110" ++ rd ++ ra ++ i
encode "brz"  (C rd i)     = Right $ "11111" ++ rd ++ i

-- Implicit Instructions --
encode "ld"   (H rd ra)    = Right $ "10100" ++ rd ++ ra ++ "00000"
encode "st"   (H rd ra)    = Right $ "10110" ++ rd ++ ra ++ "00000"
encode "lea"  (H rd ra)    = Right $ "11000" ++ rd ++ ra ++ "00000"
encode "call" (H rd ra)    = Right $ "11010" ++ rd ++ ra ++ "00000"
encode "brz"  (H rd ra)    = Right $ "11110" ++ rd ++ ra ++ "00000"
encode "call" (G ra i)     = Right $ "11010101" ++ ra ++ i
encode "call" (J ra)       = Right $ "11010101" ++ ra ++ "00000"
encode "call" (F i)        = Right $ "11011101" ++ i

-- Fabricated Instructions --
encode "nop"  (E)          = Right $ "0000000000000000"
encode "set"  (G rd i)     = Right $ "11000" ++ rd ++ "111" ++ i
encode "mov"  (H rd ra)    = Right $ "11000" ++ rd ++ ra ++ "00000"
encode "br"   (G ra i)     = Right $ "11010111" ++ ra ++ i
encode "br"   (J ra)       = Right $ "11010111" ++ ra ++ "00000"
encode "br"   (F i)        = Right $ "11011111" ++ i
encode "ret"  (E)          = Right $ "1101011110100000"
encode "halt" (E)          = Right $ "1111111111111111"

-- Directives --
encode ".word" (I i)       = Right $ i

-- Default Error --
encode op args = Left $ "Invalid instruction: " ++ show op ++ " with " ++ show args


