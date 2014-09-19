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


main = do
    args <- getArgs
    let config = opt args defConfig

    hin <- if isNothing (infile config)
           then return stdin
           else openFile (fromJust $ infile config) ReadMode

    hout <- if isNothing (outfile config)
            then return stdout
            else openFile (fromJust $ outfile config) WriteMode

    input <- hGetContents hin
    -- TODO assemble
    hPutStr hout input

    hClose hout
    hClose hin

