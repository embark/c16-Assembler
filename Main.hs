module Main where

import Assembler
import System.Environment
import System.IO
import Data.Maybe

data Config = Config { 
    mode :: String,
    outtype :: String,
    infile :: Maybe FilePath,
    outfile :: Maybe FilePath
} deriving (Show)

defConfig :: Config
defConfig = Config {
    mode = "bin",
    outtype = "bin",
    infile = Nothing,
    outfile = Nothing
}

opt :: [String] -> Config -> Config
opt [] c = c
opt ("-m16":ps) c = opt ps $ c { mode = "m16" }
opt ("-m32":ps) c = opt ps $ c { mode = "m32" }
opt ("-t":n:ps) c = opt ps $ c { outtype = n }
opt ("-o":n:ps) c = opt ps $ c { outfile = Just n }
opt (n:ps) c = opt ps $ c { infile = Just n }

use :: Maybe FilePath -> IOMode -> (Handle -> IO ()) -> IO ()
use Nothing ReadMode = ($ stdin)
use Nothing WriteMode = ($ stdout)
use (Just file) mode = withFile file mode

main = do
    args <- getArgs
    let config = opt args defConfig

    use (infile config) ReadMode $ \hin -> do
    use (outfile config) WriteMode $ \hout -> do
        input <- hGetContents hin

        case assemble input config of
            Left err -> ioError $ userError err
            Right mc -> hPutStr hout mc

