{-# LANGUAGE ViewPatterns #-}

import Data.Word
import Data.Maybe
import Data.Char
import Data.Bits
import Control.Applicative

data Var = Imm Word8 | RegID Word8 deriving (Show)
data Assembly = Assembly {ins :: Instruction, vars :: [String]}

type MachineCode = String
type Instruction = String
type Error = String

registers :: [(String, Var)]
registers = [("z", RegID 7)] ++ [("r" ++ show r, RegID r) | r <- [0..7]]

assemble :: String -> Either Error MachineCode
assemble s = getIns $ Assembly instruction vars
    where (instruction:vars) = words . addSpaces $ s
    
getIns :: Assembly -> Either Error MachineCode
getIns (Assembly (getOp -> Right opCode) (getVarCode -> Right varCode)) = Right $ opCode ++ varCode
getIns (Assembly (getOp -> Left errStr) _) = Left errStr
getIns (Assembly _ (getVarCode -> Left errStr)) = Left errStr
getIns _ = Left $ "Invalid Instruction"

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
    Right [RegID rd, RegID ra, RegID rb] -> Right $ "1" ++ getReg rd ++ getReg ra ++ "00" ++ getReg rb
    Right [RegID rd, RegID ra, Imm imm5] -> Right $ "0" ++ getReg rd ++ getReg ra ++ getImm5 imm5
    Right [RegID rd, Imm imm8] -> Right $ "1" ++ getReg rd ++ getImm8 imm8
    Right _ -> Left $ "Invalid Instruction"
    Left errStr -> Left errStr

getReg = getNLowestBits 3
getImm5 = getNLowestBits 5
getImm8 = getNLowestBits 8

getNLowestBits :: Int -> Word8 -> String
getNLowestBits n word8 = map (boolToBit . testBit word8) [n - 1, n - 2 .. 0]
    where boolToBit bool = if bool then '1' else '0'

getVar :: String -> Either String Var
getVar sym@(s:ss)
    | isJust reg = Right $ fromJust reg
    | s == '-' = Imm . negate <$> readImm ss
    | s == '+' = Imm <$> readImm ss
    | isDigit s = Imm <$> readImm sym
    | otherwise = Left $ "Invalid symbol: " ++ sym
    where reg = lookup sym registers

readImm :: Num a => String -> Either String a
readImm ('0':'x':imm) = readImm' isHexDigit 16 imm
readImm ('0':'o':imm) = readImm' isOctDigit 8 imm
readImm ('0':'b':imm) = readImm' (`elem` "01") 2 imm
readImm imm = readImm' isDigit 10 imm

readImm' :: Num a => (Char -> Bool) -> a -> String -> Either String a
readImm' pred base imm
    | all pred digs = Right $ foldl (\l r -> base*l + r) 0 nums
    | otherwise     = Left $ "Invalid immediate: " ++ imm
    where nums = map (fromIntegral . digitToInt) digs
          digs = filter (/= '_') imm

addSpaces :: String -> String
addSpaces [] = []
addSpaces (',':ss) = ' ' : addSpaces ss
addSpaces ('+':ss) = ' ' : '+' : addSpaces ss
addSpaces ('-':ss) = ' ' : '-' : addSpaces ss
addSpaces (s:ss) = s : addSpaces ss

main = do
    instructions <- getContents
    let results = mapM assemble $ lines instructions
    case results of
        Left e -> error e
        Right machineList -> mapM_ putStrLn machineList
