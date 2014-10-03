{-# LANGUAGE ViewPatterns #-}

module Main where

import Assembler
import System.Environment
import System.IO
import Data.Maybe
import Data.List

data Config = Config {
    isHelp :: Bool,
    mode :: String,
    outtype :: String,
    infile :: Maybe FilePath,
    outfile :: Maybe FilePath,
    mifSize :: Int
} deriving (Show)

defConfig :: Config
defConfig = Config {
    isHelp = False,
    mode = "m16",
    outtype = "mif",
    infile = Nothing,
    outfile = Nothing,
    mifSize = 1024
}

help :: String -> String
help prog = "\
    \Usage: " ++ prog ++ " [options] <file>\n\
    \Options:\n\
    \  --help                 Display this help message and exit\n\
    \  -m16                   Generate 16-bit code (default)\n\
    \  -m32                   Generate 32-bit code\n\
    \  -o <output>            Place the output into <file>\n\
    \  -t [bin,hex,mif,emb]   Specify type of output (default mif)\n\
    \  --size=<int>, -s <int> Max instructions (default 1024 for mif)\n\
    \ \n"

opt :: [String] -> Config -> Config
opt [] c = c
opt ("--help":ps) c = opt ps $ c { isHelp = True }
opt ("-m16":ps) c = opt ps $ c { mode = "m16" }
opt ("-m32":ps) c = opt ps $ c { mode = "m32" }
opt ("-t":n:ps) c = opt ps $ c { outtype = n }
opt ("-o":n:ps) c = opt ps $ c { outfile = Just n }
opt ("-s":n:ps) c = opt ps $ c { mifSize = read n }
opt ((stripPrefix "--size=" -> Just n):ps) c = opt ps $ c { mifSize = read n }
opt (n:ps) c = opt ps $ c { infile = Just n }

use :: Maybe FilePath -> IOMode -> (Handle -> IO ()) -> IO ()
use Nothing ReadMode = ($ stdin)
use Nothing WriteMode = ($ stdout)
use (Just file) mode = withFile file mode

assemblerFor :: Config -> (String -> Either Error [MachineCode])
assemblerFor config = case (outtype config) of
    "bin" -> assemble
    "hex" -> assembleToHex
    "emb" -> assembleToEmbed
    "mif" -> assembleToMif (mifSize config)

main = do
    args <- getArgs
    let config = opt args defConfig

    if isHelp config then do
        prog <- getProgName
        putStr $ help prog
    else do
        assembleFiles config


assembleFiles config = do
    let assembler = assemblerFor config

    use (infile config) ReadMode $ \hin -> do
    use (outfile config) WriteMode $ \hout -> do
        input <- hGetContents hin

        case assembler input of
            Left e   -> error e
            Right mc -> mapM_ (hPutStrLn hout) mc

