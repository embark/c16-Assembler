{-# LANGUAGE ViewPatterns #-}

module Assembler where

import Data.List
import Data.Int
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Either
import Control.Applicative
import Control.Monad
import Numeric

data Format = A String | B String | C String | Invalid
data Var = Imm Int16 | RegID Int16 deriving (Show)

type MachineCode = String
type AssemblyCode = String
type Instruction = String
type Error = String


registers :: [(String, Var)]
registers = [("z", RegID 7)] ++ [("r" ++ show r, RegID r) | r <- [0..7]]

assemble :: AssemblyCode -> Either Error [MachineCode]
assemble s = filter (/= "") <$> (mapM assembleLine $ lines s)

assembleToHex :: AssemblyCode -> Either Error [MachineCode]
assembleToHex s = map toHex <$> assemble s
    where toHex [] = []
          toHex bits = (hexDigit $ take 4 bits) : (toHex $ drop 4 bits)
          hexDigit = (intToDigit . readImmBase 2)

assembleToEmbed :: AssemblyCode -> Either Error [MachineCode]
assembleToEmbed asm = formatEmbed assemblyLines <$> assembleToHex asm
    where assemblyLines = filter (/= "") . map cleanLine $ lines asm

formatEmbed :: [AssemblyCode] -> [MachineCode] -> [String]
formatEmbed assembly codes =
    ["always@(*) begin"] ++
    ["       case(pc)"] ++
                      zipWith formatCodeLine codes [0..] ++
    ["                default: ins = 16'ffff; // halt"] ++
    ["       endcase"] ++
    ["end"]
    where formatCodeLine code pc =
            "\t\t16'd" ++ (show pc) ++ ": ins = 16'h" ++ code ++ ";" ++
            " // " ++ (assembly !! pc)

assembleLine :: AssemblyCode -> Either String MachineCode
assembleLine (words . addSpaces . cleanLine -> tokens)
    | tokens == [] = Right ""
    | otherwise = case eitherVars of
            Left errStr -> Left errStr
            Right vars -> getIns (instruction, vars)
    where eitherVars = mapM getVar vars
          instruction = head $ tokens
          vars = tail $ tokens

-- Remove comments and beginning whitespace from code
cleanLine :: AssemblyCode -> AssemblyCode
cleanLine = dropWhile isSpace . takeWhile (/= '/')

getIns :: (Instruction, [Var]) -> Either Error MachineCode
getIns ("add", (getFormat -> B varCode)) = Right $ "00000" ++ varCode
getIns ("add", (getFormat -> A varCode)) = Right $ "00001" ++ varCode
getIns ("call", (getFormat -> B varCode)) = Right $ "11010" ++ varCode
getIns ("call", (getFormat -> C varCode)) = Right $ "11011" ++ varCode
getIns ("slt", (getFormat -> B varCode)) = Right $ "00100" ++ varCode
getIns ("slt", (getFormat -> A varCode)) = Right $ "00101" ++ varCode
getIns ("brz", (getFormat -> B varCode)) = Right $ "11110" ++ varCode
getIns ("brz", (getFormat -> C varCode)) = Right $ "11111" ++ varCode
getIns ("lea", (getFormat -> B varCode)) = Right $ "11000" ++ varCode
getIns ("lea", (getFormat -> C varCode)) = Right $ "11001" ++ varCode
getIns ("shl", (getFormat -> B varCode)) = Right $ "10000" ++ varCode
getIns ("shl", (getFormat -> A varCode)) = Right $ "10001"++ varCode
getIns (_, (getEitherFormat -> Left err)) = Left err
getIns asm = Left $ "Invalid instruction: " ++ show asm
    
getFormat :: [Var] -> Format
getFormat (getEitherFormat -> Right fmt) = fmt
getFormat (getEitherFormat -> Left err) = Invalid

getEitherFormat :: [Var] -> Either Error Format
getEitherFormat [RegID rd, RegID ra, RegID rb] = A <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getReg ra <*> getReg rb
              buildBits dBits aBits bBits = dBits ++ aBits ++ "00" ++ bBits
getEitherFormat [RegID rd, RegID ra, Imm imm5] = B <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getReg ra <*> getImm5 imm5
              buildBits dBits aBits immBits = dBits ++ aBits ++ immBits
getEitherFormat [RegID rd, Imm imm8] = C <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getImm8 imm8
              buildBits dBits immBits = dBits ++ immBits
getEitherFormat vars = Left $ "Invalid format for vars: " ++ show vars

getReg = getNLowestBits 3 False
getImm5 = getNLowestBits 5 True
getImm8 = getNLowestBits 8 True

getNLowestBits :: Int -> Bool -> Int16 -> Either Error String
getNLowestBits n isSigned int16
    | tooManyBits = Left $ errMsg
    | otherwise = Right $ map (boolToBit . testBit int16) [n - 1, n - 2 .. 0]
    where boolToBit bool = if bool then '1' else '0'
          tooManyBits 
            | isSigned = not (int16 `elem` [(negate (2^(n-1)))..(2^(n-1))])
            | otherwise = not (int16 `elem` [0..2^n])
          errMsg = "More than " ++ (show n) ++ " bits encode: " ++ (show int16)

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

