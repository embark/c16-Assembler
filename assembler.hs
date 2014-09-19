{-# LANGUAGE ViewPatterns #-}

import Data.Word
import Data.Bits
import Control.Applicative

data Format = A String | B String | C String | Invalid deriving (Show)
data Var = Imm Word8 | RegID Word8 deriving (Show)

type MachineCode = String
type OpCode = String

assemble :: String -> Either String MachineCode
assemble s = case eitherVars of 
    Left errStr -> Left errStr
    Right vars -> getIns (opcode, vars)
    where eitherVars = mapM getVar xs
          (opcode:xs) = words . addSpaces $ s

getIns :: (OpCode, [Var]) -> Either String MachineCode
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
getIns _ = Left "Invalid instruction"

getFormat :: [Var] -> Format
getFormat [RegID rd, RegID ra, RegID rb] = 
    A $ getReg rd ++ getReg ra ++ "00" ++ getReg rb
getFormat [RegID rd, RegID ra, Imm imm5] = 
    B $ getReg rd ++ getReg ra ++ getImm5 imm5
getFormat [RegID rd, Imm imm8] = 
    C $ getReg rd ++ getImm8 imm8
getFormat _ = Invalid

getReg = getNLowestBits 3
getImm5 = getNLowestBits 5
getImm8 = getNLowestBits 8

getNLowestBits :: Int -> Word8 -> String
getNLowestBits n word8 = map (boolToBit . testBit word8) [n - 1, n - 2 .. 0]
    where boolToBit bool = if bool then '1' else '0'

getVar :: String -> Either String Var
getVar ('r':regVal) = RegID <$> readEither regVal
getVar ('$':imm) = Imm <$> readEither imm
getVar wrong = Left $ "Invalid symbol: " ++ wrong

readEither :: (Read a) => String -> Either String a  
readEither st = case reads st of [(x,"")] -> return x  
                                 _ -> Left $ "Could not parse: " ++ st  

addSpaces :: String -> String
addSpaces = map (\x -> if x == ',' || x == ' '  || x == '+' then ' ' else x) 

main = do
    instructions <- getContents
    let results = mapM assemble $ lines instructions
    case results of
        Left e -> error e
        Right machineList -> mapM_ putStrLn machineList
