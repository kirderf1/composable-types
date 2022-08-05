module Main where 

import Control.Monad.Except (when, void, runExceptT)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

import Language.Haskell.Exts (Module, SrcSpanInfo, ParseResult(..), prettyPrint, parseFile)

import Transform
import TransformFiles

-- | Parse arguments from command line and perform a transformation on the file(s) provided,
-- writing the result to standard output.
-- If there is a single input file, a debug flag can be provided to also print intermediate
-- states of parse and transformation.
main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  let file = inputFile args
  if (null file) 
     then runDir args
     else do
    res <- parseFile file
    case res of 
        f@ParseFailed{} -> error $ show f
        ParseOk ast -> runTests ast (debugPrint args)
    where
        runTests :: Module SrcSpanInfo -> Bool -> IO ()
        runTests ast debug = do
            when debug
                $ do putStrLn "AST structure before:"
                     showModule ast
                     putStrLn "Pretty-print before:"
                     putStrLn $ prettyPrint ast
            result <- runExceptT (transform ast)
            case result of
                Left msg -> error msg
                Right ast' -> if debug
                                then do putStrLn "AST structure after:"
                                        showModule ast'
                                        putStrLn "Pretty-print after:"
                                        putStrLn $ prettyPrint ast'
                                else putStrLn $ prettyPrint ast'
                          
showModule :: Module l -> IO ()
showModule = print . void

-- | Run transformation on files given in a directory, writing the output to a specified directory.
runDir :: Args -> IO ()
runDir args = do
    let inDir = inputDir args
        outDir = outputDir args
    when (null inDir) $ error "No input file nor input directory"
    when (null outDir) $ error "No output directory"
    res <- runExceptT (transpileFilesFromDir inDir outDir)
    case res of
         Left msg -> putStrLn msg
         Right _ -> return ()


data Args = Args
  { inputFile :: FilePath
  , inputDir :: FilePath
  , outputDir :: FilePath
  , debugPrint :: Bool
  }

-- | Parse arguments from a list of strings.
-- Provide either path to a Haskell file to be transformed, or paths to two directories.
-- The first one is the directory of files to be transformed, the second the where to put the resulting files.
-- --debug can be provided for single file input to print intermediate states. Write -- --debug in command line.
parseArgs :: [String] -> Args
parseArgs ("--debug" : rest) = case parseArgs rest of
         arg | not (debugPrint arg) -> arg{debugPrint = True}
         _                     -> error "Duplicate debug flag"
parseArgs (flag@('-':_) : _) = error ("Unknown flag: " ++ flag)
parseArgs (path : rest) | isFile path = case parseArgs rest of
         arg | null $ inputFile arg -> arg{inputFile = path}
         _                          -> error "Multiple input files are not supported"
                       | otherwise   = case parseArgs rest of
         arg | null $ outputDir arg -> arg{outputDir = path}
         arg | null $ inputDir arg  -> arg{inputDir = path}
         _                          -> error "Expected only two directories, got more"
    where isFile :: FilePath -> Bool
          isFile = (== ".hs") . takeExtension 
parseArgs [] = Args {inputFile = "", inputDir = "", outputDir = "", debugPrint = False}


