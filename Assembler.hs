{-# LANGUAGE ViewPatterns #-}

module Assembler where

import Data.List
import Data.Int
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Either
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Numeric

data Format = A String String String
            | B String String String
            | C String String
            | E
            | F String
            | G String String
            | H String String
            | I String
            | J String
    deriving (Show)

data Var = Imm Int | RegID Int | Label Int deriving (Show)

type MachineCode = String
type AssemblyCode = String
type Instruction = String
type Error = String
type LabelMap = M.Map String Int


registers :: [(String, Var)]
registers = [("z", RegID 7)] ++ [("r" ++ show r, RegID r) | r <- [0..7]]

assemble :: AssemblyCode -> Either Error [MachineCode]
assemble s = filter (/= "") <$> (zipWithM (assembleLine labels) [1..] $ codeLines)
    where labels = getLabels cleanLines
          cleanLines = filter isLine $ map cleanLine $ lines $ s
          codeLines = filter isLine $ map deLabel $ cleanLines

assembleToHex :: AssemblyCode -> Either Error [MachineCode]
assembleToHex s = map toHex <$> assemble s
    where toHex [] = []
          toHex bits = (hexDigit $ take 4 bits) : (toHex $ drop 4 bits)
          hexDigit = (intToDigit . readImmBase 2)

assembleToEmbed :: AssemblyCode -> Either Error [MachineCode]
assembleToEmbed asm = formatEmbed <$> assembleToHex asm

formatEmbed :: [MachineCode] -> [String]
formatEmbed codes =
    ["always@(*) begin"] ++
    ["       case(pc)"] ++
                      zipWith formatCodeLine codes [0..] ++
    ["                default: ins = 16'ffff; // halt"] ++
    ["       endcase"] ++
    ["end"]
    where formatCodeLine code pc =
            "\t\t16'd" ++ (show pc) ++ ": ins = 16'h" ++ code ++ ";"

assembleToMif :: AssemblyCode -> Either Error [MachineCode]
assembleToMif asm = formatMif <$> assembleToHex asm

formatMif :: [MachineCode] -> [String]
formatMif codes =
    ["Width=16;"] ++
    ["Depth=1024;\n"] ++
    ["ADDRESS_RADIX=DEC;"] ++
    ["DATA_RADIX=HEX;\n"] ++
    ["CONTENT BEGIN"] ++
        zipWith formatCodeLine codes [0..] ++
        ["\t[" ++ (show (length codes))  ++ "..1023]: ffff;"] ++
    ["END;"]
    where formatCodeLine code pc = "\t" ++ (show pc) ++ ": " ++ code ++ ";"

assembleLine :: LabelMap -> Int -> AssemblyCode -> Either String MachineCode
assembleLine labels pc line@(words . addSpaces -> tokens) = 
    case eitherVars of
            Left errStr -> Left errStr
            Right vars -> getIns pc instruction vars
    where eitherVars = mapM (getVar labels) vars
          instruction = head $ tokens
          vars = tail $ tokens

-- Remove comments and beginning whitespace from code
cleanLine :: AssemblyCode -> AssemblyCode
cleanLine = dropWhile isSpace . takeWhile (`notElem` "/;")

isLine :: AssemblyCode -> Bool
isLine = not . null . words

-- Checks for labels in assembly
deLabel :: AssemblyCode -> AssemblyCode
deLabel = unwords . dropWhile (isJust.getLabel) . words

getLabels :: [AssemblyCode] -> LabelMap
getLabels (x:xs) = getLabels' 0 (words x) xs M.empty
getLabels [] = M.empty

getLabels' :: Int -> [AssemblyCode] -> [AssemblyCode] -> LabelMap -> LabelMap
getLabels' pc ((getLabel -> Just label):next) rest map =
    getLabels' pc next rest $ M.insert label pc map
getLabels' pc [] (next:rest) map = getLabels' pc (words next) rest map
getLabels' pc _ (next:rest) map = getLabels' (pc + 1) (words next) rest map
getLabels' _ _ [] map = map

getLabel :: AssemblyCode -> Maybe AssemblyCode
getLabel code
    | last code == ':' = Just (init code)
    | otherwise = Nothing

getIns :: Int -> Instruction -> [Var] -> Either Error MachineCode
getIns pc ins vars
    | null $ rights encodings = last encodings
    | otherwise               = Right $ head $ rights encodings
    where encodings = map (either Left (encode ins)) formats
          formats = getFormats pc vars

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

getFormats :: Int -> [Var] -> [Either Error Format]
getFormats _ [RegID rd, RegID ra, RegID rb] = [tryParse]
    where tryParse = A <$> getReg rd <*> getReg ra <*> getReg rb
getFormats _ [RegID rd, RegID ra, Imm imm5] = [tryParse]
    where tryParse = B <$> getReg rd <*> getReg ra <*> getImm5 imm5
getFormats _ [RegID rd, Imm imm] = [tryParseG, tryParseC]
    where tryParseC = C <$> getReg rd <*> getImm8 imm
          tryParseG = G <$> getReg rd <*> getImm5 imm
getFormats _ [Imm imm] = [tryParseF, tryParseI]
    where tryParseF = F <$> getImm8 imm
          tryParseI = I <$> getNLowestBits 16 False imm
getFormats _ [RegID rd, RegID ra] = [tryParse]
    where tryParse = H <$> getReg rd <*> getReg ra
getFormats _ [RegID ra] = [tryParse]
    where tryParse = J <$> getReg ra
getFormats pc [RegID rd, Label labelPc] = getFormats pc [RegID rd, Imm labelOffset]
    where labelOffset = labelPc - pc
getFormats pc [Label labelPc] = getFormats pc [Imm labelOffset]
    where labelOffset = labelPc - pc
getFormats _ [] = [Right E]
getFormats _ vars = [Left $ "Invalid format for vars: " ++ show vars]

getReg = getNLowestBits 3 False
getImm5 = getNLowestBits 5 True
getImm8 = getNLowestBits 8 True

getNLowestBits :: Int -> Bool -> Int -> Either Error String
getNLowestBits n isSigned val
    | tooManyBits = Left $ errMsg
    | otherwise = Right $ map (boolToBit . testBit val) [n - 1, n - 2 .. 0]
    where boolToBit bool = if bool then '1' else '0'
          tooManyBits 
            | isSigned = not (val `elem` [(negate (2^(n-1)))..(2^(n-1))])
            | otherwise = not (val `elem` [0..2^n])
          errMsg = "More than " ++ (show n) ++ " bits encode: " ++ (show val)

getVar :: LabelMap -> String -> Either String Var
getVar labels sym@(s:ss)
    | isJust reg = Right $ fromJust reg
    | s == '-' = Imm . negate <$> readImm ss
    | s == '+' = Imm <$> readImm ss
    | isJust maybePc = Right $ Label (fromJust maybePc)
    | isDigit s = Imm <$> readImm sym
    | otherwise = Left $ "Invalid symbol: " ++ sym
    where reg = lookup sym registers
          maybePc = getLabel (sym ++ ":") >>= flip M.lookup labels

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

