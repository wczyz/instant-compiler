module Runner (InputData, prepareAST) where

import qualified Data.Text.IO as TIO
import Parser
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)

data InputData = InputData
    { _filename :: Maybe String
    , _program :: Maybe Program
    }

prepareAST :: IO InputData
prepareAST = do
    args <- getArgs

    -- read from file if specified
    -- otherwise read from stdin
    input <- case args of
        (filename : _) -> TIO.readFile filename
        _ -> TIO.getContents

    let filename = 
            case args of
                (x:_) -> Just x
                _ -> Nothing

    program <- case parse pProg "" input of
        (Right v) -> return (Just v)
        (Left e) -> do
            putStrLn $ errorBundlePretty e 
            return Nothing

    return $ InputData{_filename = filename, _program = program}
