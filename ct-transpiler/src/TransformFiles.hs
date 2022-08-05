module TransformFiles (transpileFilesFromDir, transpileFiles) where

import Language.Haskell.Exts
import Language.Haskell.Names (Environment, readSymbols, writeSymbols, resolve, loadBase)

import Transform
import TransformUtils

import System.FilePath
import System.Directory

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except
import Data.Functor.Identity

-- | Transpile all files in a directory and put the transpiled files in a given output directory
transpileFilesFromDir :: FilePath -> FilePath -> ExceptT String IO ()
transpileFilesFromDir dir outdir = 
    if dir == outdir
       then throwError "Output directory cannot be the same as the directory with files to be transpiled"
       else do
        exists <- lift $ doesDirectoryExist dir
        if exists
        then do
            contents <- lift $ getDirectoryContents dir
            let files = filter (\f -> takeExtension f == ".hs") contents
            transpileFiles outdir (map (dir </>) files )
        else throwError "No such directory"

-- | Transpile a list of files and put the transpiled files in a given output directory
transpileFiles :: FilePath -> [FilePath] -> ExceptT String IO ()
transpileFiles outdir files = do
    modules <- parseModuleFromFile `mapM` files
    baseEnv <- lift $ loadBase
    let env = resolve modules baseEnv
    transpileFile env outdir `mapM_` modules

parseModuleFromFile :: FilePath -> ExceptT String IO (Module SrcSpanInfo)--TODO rename
parseModuleFromFile file = do
    parseResult <- lift $ parseFile file
    case parseResult of
        f@ParseFailed{} -> throwError $ show f
        ParseOk m       -> return m

-- | Transpile a single file by also checking their imports and transpiling them if necessary
transpileFile :: Environment -> FilePath -> Module SrcSpanInfo -> ExceptT String IO ()
transpileFile env outdir m@(Module _ _mhead _pragmas imports _decls) = do
    env <- foldM (loadSymbolsToEnv outdir) env missingImports
    ast' <- fromExcept (transform' env m)
    let result = prettyPrint ast'
    lift $ createDirectoryIfMissing True outdir
    lift $ writeFile (outdir </> fileName <.> "hs") $ result ++ "\n"
    case Map.lookup moduleName env of
      Just symbols -> lift $ writeSymbols (outdir </> fileName <.> "symbols") symbols
      Nothing      -> return ()
  where
    moduleName = getModuleName m
    fileName = prettyPrint moduleName
    missingImports = do
        ImportDecl{importModule = moduleName} <- imports
        if Map.member (void moduleName) env then [] else [void moduleName]

loadSymbolsToEnv :: FilePath -> Environment -> ModuleName () -> ExceptT String IO Environment
loadSymbolsToEnv dir env moduleName = do
    let infoFile = dir </> prettyPrint moduleName <.> ".symbols"
    existsInfo <- lift $ doesFileExist infoFile
    if existsInfo
        then do
            content <- lift $ readSymbols infoFile
            return $ Map.insert moduleName content env
        else
            throwError $ "Unable to find symbols for module " ++ prettyPrint moduleName ++ " at " ++ show infoFile

