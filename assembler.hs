import Data.List
import Data.Word
import Data.Char
import Data.Maybe
import Numeric
import Control.Applicative

data Format = A String | B String | C String | Invalid deriving (Show)
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
    let results = map assemble $ lines instructions
    case sequence results of
        Left e -> ioError $ userError e
        Right machineList -> mapM_ putStrLn machineList
