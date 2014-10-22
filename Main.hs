{-# LANGUAGE ViewPatterns #-}

module Main where

import Assembler
import System.Environment
import System.IO
import Data.Maybe
import Data.List

data Config = Config {
    isHelp :: Bool,
    mode :: Mode,
    outtype :: String,
    infile :: Maybe FilePath,
    outfile :: Maybe FilePath
} deriving (Show)

defConfig :: Config
defConfig = Config {
    isHelp = False,
    mode = M16,
    outtype = "mif",
    infile = Nothing,
    outfile = Nothing
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
    \ \n"

opt :: [String] -> Config -> Config
opt [] c = c
opt ("--help":ps) c = opt ps $ c { isHelp = True }
opt ("-m16":ps) c = opt ps $ c { mode = M16 }
opt ("-m32":ps) c = opt ps $ c { mode = M32 }
opt ("-t":n:ps) c = opt ps $ c { outtype = n }
opt ("-o":n:ps) c = opt ps $ c { outfile = Just n }
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
    "mif" -> assembleToMif (mode config)

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

