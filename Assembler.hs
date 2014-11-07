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

data Format = A String | B String | C String | Invalid
data Var = Imm Int | RegID Int | Label Int deriving (Show)
data Mode = M16 | M32 deriving (Show)

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

assembleToMif :: Mode -> AssemblyCode -> Either Error [MachineCode]
assembleToMif mode asm = do
    codeLines <- assembleToHex asm
    return $ formatMif mode codeLines

formatMif :: Mode -> [MachineCode] -> [String]
formatMif M16 codes = formatMifWidth 16 codes
formatMif M32 codes = formatMifWidth 32 (to32bit codes)

formatMifWidth :: Int -> [MachineCode] -> [String]
formatMifWidth width codes =
    ["Width=" ++ show width ++ ";"] ++
    ["Depth=" ++ (show $ (length codes + 1)) ++ ";\n"] ++
    ["ADDRESS_RADIX=DEC;"] ++
    ["DATA_RADIX=HEX;\n"] ++
    ["CONTENT BEGIN"] ++
        zipWith formatCodeLine codes [0..] ++
        [defaultLine] ++
    ["END;"]
    where formatCodeLine code pc = "\t" ++ (show pc) ++ ": " ++ code ++ ";"
          defaultLine
            | width == 16 = "\t" ++ (show $ length codes)  ++ ": " ++ "ffff;"
            | width == 32 = "\t" ++ (show $ length codes)  ++ ": " ++ "ffffffff;"


to32bit :: [MachineCode] -> [MachineCode]
to32bit (x:y:xs) = (x ++ y) : to32bit xs
to32bit [x] = [x ++ "ffff"]
to32bit [] = []

assembleLine :: LabelMap -> Int -> AssemblyCode -> Either String MachineCode
assembleLine labels pc line@(words . addSpaces -> tokens) = 
    case eitherVars of
            Left errStr -> Left errStr
            Right vars -> getIns pc (instruction, vars)
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

getIns :: Int -> (Instruction, [Var]) -> Either Error MachineCode
getIns pc ("add", (getFormat pc -> B varCode)) = Right $ "00000" ++ varCode
getIns pc ("add", (getFormat pc -> A varCode)) = Right $ "00001" ++ varCode
getIns pc ("sub", (getFormat pc -> B varCode)) = Right $ "00010" ++ varCode
getIns pc ("sub", (getFormat pc -> A varCode)) = Right $ "00011" ++ varCode
getIns pc ("slt", (getFormat pc -> B varCode)) = Right $ "00100" ++ varCode
getIns pc ("slt", (getFormat pc -> A varCode)) = Right $ "00101" ++ varCode
getIns pc ("shl", (getFormat pc -> B varCode)) = Right $ "10000" ++ varCode
getIns pc ("shl", (getFormat pc -> A varCode)) = Right $ "10001"++ varCode
getIns pc ("call", (getFormat pc -> B varCode)) = Right $ "11010" ++ varCode
getIns pc ("call", (getFormat pc -> C varCode)) = Right $ "11011" ++ varCode
getIns pc ("brz", (getFormat pc -> B varCode)) = Right $ "11110" ++ varCode
getIns pc ("brz", (getFormat pc -> C varCode)) = Right $ "11111" ++ varCode
getIns pc ("brnz", (getFormat pc -> B varCode)) = Right $ "11100" ++ varCode
getIns pc ("brnz", (getFormat pc -> C varCode)) = Right $ "11101" ++ varCode
getIns pc ("lea", (getFormat pc -> B varCode)) = Right $ "11000" ++ varCode
getIns pc ("lea", (getFormat pc -> C varCode)) = Right $ "11001" ++ varCode
getIns pc ("ld", (getFormat pc -> B varCode)) = Right $ "10100" ++ varCode
getIns pc ("ld", (getFormat pc -> C varCode)) = Right $ "10101" ++ varCode
getIns pc ("st", (getFormat pc -> B varCode)) = Right $ "10110" ++ varCode
getIns pc ("st", (getFormat pc -> C varCode)) = Right $ "10111" ++ varCode
getIns pc (".word", [Imm val]) = getNLowestBits 16 False val
getIns pc (_, (getEitherFormat pc -> Left err)) = Left err
getIns _ asm = Left $ "Invalid instruction: " ++ show asm
    
getFormat :: Int -> [Var] -> Format
getFormat pc (getEitherFormat pc -> Right fmt) = fmt
getFormat pc (getEitherFormat pc -> Left err) = Invalid

getEitherFormat :: Int -> [Var] -> Either Error Format
getEitherFormat _ [RegID rd, RegID ra, RegID rb] = A <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getReg ra <*> getReg rb
              buildBits dBits aBits bBits = dBits ++ aBits ++ "00" ++ bBits
getEitherFormat _ [RegID rd, RegID ra, Imm imm5] = B <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getReg ra <*> getImm5 imm5
              buildBits dBits aBits immBits = dBits ++ aBits ++ immBits
getEitherFormat _ [RegID rd, Imm imm8] = C <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getImm8 imm8
              buildBits dBits immBits = dBits ++ immBits
getEitherFormat pc [RegID rd, Label labelPc] = C <$> tryParse
        where tryParse = buildBits <$> getReg rd <*> getImm8 labelOffset
              labelOffset = labelPc - pc
              buildBits dBits immBits = dBits ++ immBits
getEitherFormat _ vars = Left $ "Invalid format for vars: " ++ show vars

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

