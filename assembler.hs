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

registers :: [(String, Var)]
registers = [("z", RegID 7)] ++ [("r" ++ show r, RegID r) | r <- [0..7]]

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
readImm ('0':'b':imm) = readImm' (\i -> i=='1' || i=='0') 2 imm
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
    let results = map assemble $ lines instructions
    case sequence results of
        Left e -> ioError $ userError e
        Right machineList -> mapM_ putStrLn machineList
