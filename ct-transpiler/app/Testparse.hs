module Main where 

import Language.Haskell.Exts
import Transform
import Control.Monad.Except
import System.Environment
import System.FilePath

import TransformFiles

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
            case runExcept (transform $ void ast) of
                Left msg -> error msg
                Right ast' -> if debug
                                then do putStrLn "AST structure after:"
                                        showModule ast'
                                        putStrLn "Pretty-print after:"
                                        putStrLn $ prettyPrint ast'
                                else putStrLn $ prettyPrint ast'
                          
showModule :: Module l -> IO ()
showModule = print . void

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


