module Main where 

import Language.Haskell.Exts
import Transform
import Control.Monad.Except
import System.Environment


import TransformFiles

main :: IO ()
main = do
    files <- getArgs
    let (dir: outdir: []) = files
    res <- runExceptT (transpileFilesFromDir dir outdir)
    case res of
         Left msg -> putStrLn msg
         Right _ -> return ()

-- main :: IO ()
-- main = do
--   arg <- parseArgs <$> getArgs
--   let file = inputFile arg
--   when (null file) (error "Missing input file")
--   res <- parseFile file
--   case res of 
--       f@ParseFailed{} -> error $ show f
--       ParseOk ast -> runTests ast (debugPrint arg)
--   where
--     runTests :: Module SrcSpanInfo -> Bool -> IO ()
--     runTests ast debug = do
--       when debug
--         $ do putStrLn "AST structure before:"
--              showModule ast
--              putStrLn "Pretty-print before:"
--              putStrLn $ prettyPrint ast
--       case runExcept (transform $ void ast) of
--          Left msg -> error msg
--          Right ast' -> if debug
--                           then do putStrLn "AST structure after:"
--                                   showModule ast'
--                                   putStrLn "Pretty-print after:"
--                                   putStrLn $ prettyPrint ast'
--                           else putStrLn $ prettyPrint ast'

showModule :: Module l -> IO ()
showModule = print . void


data Args = Args
  { inputFile :: FilePath
  , debugPrint :: Bool
  }

parseArgs :: [String] -> Args
parseArgs ("--debug" : rest) = case parseArgs rest of
         arg | not (debugPrint arg) -> arg{debugPrint=True}
         _                     -> error "Duplicate debug flag"
parseArgs (flag@('-':_) : _) = error ("Unknown flag: " ++ flag)
parseArgs (file : rest) = case parseArgs rest of
         arg | null $ inputFile arg -> arg{inputFile=file}
         _                          -> error "Multiple input files are not supported"
parseArgs [] = Args {inputFile = "", debugPrint = False}

