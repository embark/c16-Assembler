{-# LANGUAGE ViewPatterns #-}

module Assembler where

import Data.List
import Data.Int
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Either
import Control.Applicative
import Numeric


data Var = Imm Int16 | RegID Int16 deriving (Show)
data Assembly = Assembly {ins :: Instruction, vars :: [String]} deriving (Show)

type MachineCode = String
type Instruction = String
type Error = String


registers :: [(String, Var)]
registers = [("z", RegID 7)] ++ [("r" ++ show r, RegID r) | r <- [0..7]]

assemble :: String -> Either Error [MachineCode]
assemble s = mapM assembleLine $ lines s

assembleToHex :: String -> Either Error [MachineCode]
assembleToHex s = either Left (Right . map toHex) $ assemble s
    where toHex [] = []
          toHex bits = (hexDigit $ take 4 bits) : (toHex $ drop 4 bits)
          hexDigit = (intToDigit . readImmBase 2)

assembleLine :: String -> Either Error MachineCode
assembleLine s = getIns $ Assembly instruction vars
    where (instruction:vars) = words . addSpaces $ s
    
getIns :: Assembly -> Either Error MachineCode
getIns (Assembly (getOp -> Right opCode) (getVarCode -> Right varCode)) = Right $ opCode ++ varCode
getIns (Assembly (getOp -> Left errStr) _) = Left errStr
getIns asm@(Assembly _ (getVarCode -> Left errStr)) = Left $ "Invalid Instruction" ++ show asm ++ " error: " ++ errStr
getIns asm = Left $ "Invalid Instruction: " ++ show asm

getOp :: Instruction -> Either Error MachineCode
getOp "add" = Right "0000"
getOp "call" = Right "1101"
getOp "slt" = Right "0010"
getOp "brz" = Right "1111"
getOp "lea" = Right "1100"
getOp "shl" = Right "1000"
getOp malformedOp = Left $ malformedOp ++ " is not a valid instruction"

getVarCode :: [String] -> Either Error MachineCode
getVarCode vars = case mapM getVar vars of
    Right [RegID rd, RegID ra, RegID rb] -> buildBits <$> getReg rd <*> getReg ra <*> getReg rb
        where buildBits dBits aBits bBits = "1" ++ dBits ++ aBits ++ "00" ++ bBits
    Right [RegID rd, RegID ra, Imm imm5] -> buildBits <$> getReg rd <*> getReg ra <*> getImm5 imm5
        where buildBits dBits aBits immBits = "0" ++ dBits ++ aBits ++ immBits
    Right [RegID rd, Imm imm8] -> buildBits <$> getReg rd <*> getImm8 imm8
        where buildBits dBits immBits = "1" ++ dBits ++ immBits
    Right _ -> Left $ "Invalid Instruction: " ++ (show vars)
    Left errStr -> Left errStr

getReg = getNLowestBits 3 False
getImm5 = getNLowestBits 5 True
getImm8 = getNLowestBits 8 True

getNLowestBits :: Int -> Bool -> Int16 -> Either Error String
getNLowestBits n isSigned int16
    | tooManyBits = Left $ "More than " ++ (show (n)) ++ " bits required to encode: " ++ (show int16)
    | otherwise = Right $ map (boolToBit . testBit int16) [n - 1, n - 2 .. 0]
    where boolToBit bool = if bool then '1' else '0'
          tooManyBits 
            | isSigned = not (int16 `elem` [(negate (2^(n-1)))..(2^(n-1))])
            | otherwise = not (int16 `elem` [0..2^n])

getVar :: String -> Either String Var
getVar sym@(s:ss)
    | isJust reg = Right $ fromJust reg
    | s == '-' = Imm . negate <$> readImm ss
    | s == '+' = Imm <$> readImm ss
    | isDigit s = Imm <$> readImm sym
    | otherwise = Left $ "Invalid symbol: " ++ sym
    where reg = lookup sym registers

readImm :: Num a => String -> Either String a
readImm ('0':'x':imm) = readNImm isHexDigit 16 imm
readImm ('0':'o':imm) = readNImm isOctDigit 8 imm
readImm ('0':'b':imm) = readNImm (`elem` "01") 2 imm
readImm imm = readNImm isDigit 10 imm

readNImm :: Num a => (Char -> Bool) -> a -> String -> Either String a
readNImm pred base imm
    | all pred digits = Right $ readImmBase base digits
    | otherwise = Left $ "Invalid immediate: " ++ imm
    where digits = filter (/= '_') imm

readImmBase :: Num a => a -> String -> a
readImmBase base imm = foldl (\l r -> base*l + r) 0 nums
    where nums = map (fromIntegral . digitToInt) imm


addSpaces :: String -> String
addSpaces [] = []
addSpaces (',':ss) = ' ' : addSpaces ss
addSpaces ('+':ss) = ' ' : '+' : addSpaces ss
addSpaces ('-':ss) = ' ' : '-' : addSpaces ss
addSpaces (s:ss) = s : addSpaces ss

