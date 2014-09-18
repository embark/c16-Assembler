import Data.List
import Data.Word
import Data.Char
import Data.Maybe
import Numeric
import Control.Applicative

data Format = A | B | C deriving (Show)
data Var = Imm Word8 | RegID Word8 deriving (Show)

type MachineCode = String
type OpCode = String

assemble :: String -> Either String MachineCode
assemble s = case eitherVars of 
    Left error -> Left error
    Right vars -> getIns (opcode, vars)
    where eitherVars = mapM getVar xs
          (opcode:xs) = words . addSpaces $ s

getIns :: (OpCode, [Var]) -> Either String MachineCode
getIns ("add", vars@[RegID _, RegID _, Imm _]) = return $ "00000" ++ formatVars B vars
getIns ("add", vars@[RegID _, RegID _, RegID _]) = return $ "00001" ++ formatVars A vars
getIns ("call", vars@[RegID _, RegID _, Imm _]) = return $ "11010" ++ formatVars B vars
getIns ("call", vars@[RegID _, Imm _]) = return $ "11011" ++ formatVars C vars
getIns ("slt", vars@[RegID _, RegID _, Imm _]) = return $ "00100" ++ formatVars B vars
getIns ("slt", vars@[RegID _, RegID _, RegID _]) = return $ "00101" ++ formatVars A vars
getIns ("brz", vars@[RegID _, RegID _, Imm _]) = return $ "11110" ++ formatVars B vars
getIns ("brz", vars@[RegID _, Imm _]) = return $ "11111" ++ formatVars C vars
getIns ("lea", vars@[RegID _, RegID _, Imm _]) = return $ "11000" ++ formatVars B vars
getIns ("lea", vars@[RegID _, Imm _]) = return $ "11001" ++ formatVars C vars
getIns ("shl", vars@[RegID _, RegID _, Imm _]) = return $ "10000" ++ formatVars B vars
getIns ("shl", vars@[RegID _, RegID _, RegID _]) = return $ "10001"++ formatVars A vars
getIns _ = Left "Invalid instruction"

formatVars :: Format -> [Var] -> String
formatVars A [RegID rd, RegID ra, RegID rb] = 
    getReg rd ++ getReg ra ++ "00" ++ getReg rb
formatVars B [RegID rd, RegID ra, Imm imm5] =
    getReg rd ++ getReg ra ++ getImm5 imm5
formatVars C [RegID rd, Imm imm8] = 
    getReg rd ++ getImm8 imm8

getReg = getBits 3
getImm5 = getBits 5
getImm8 = getBits 8

getBits :: Int -> Word8 -> String
getBits numBits int = 
    let allBits = filter (/='"') . show $ showIntAtBase 2 intToDigit int ""
    in pad numBits . reverse . take numBits . reverse $ allBits

pad :: Int -> String -> String
pad numBits s
    | length s < numBits = pad numBits $ "0" ++ s
    | otherwise = s

getVar :: String -> Either String Var
getVar ('r':regVal) = RegID <$> readEither regVal
getVar ('$':imm) = Imm <$> readEither imm
getVar wrong = Left $ "Invalid symbol" ++ wrong

readEither :: (Read a) => String -> Either String a  
readEither st = case reads st of [(x,"")] -> return x  
                                 _ -> Left $ "Could not parse: " ++ st  

addSpaces :: String -> String
addSpaces = map (\x -> if x == ',' || x == ' '  || x == '+' then ' ' else x) 

main = do
    instructions <- getContents
    let results = map assemble $ lines instructions
    case sequence results of
        Left e -> ioError $ userError e
        Right machineList -> mapM_ putStrLn machineList
