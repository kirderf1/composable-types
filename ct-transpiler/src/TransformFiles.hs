module TransformFiles (transpileFilesFromDir, transpileFiles) where

import Language.Haskell.Exts

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
       then error "Output directory cannot be the same as the directory with files to be transpiled"
       else do
        exists <- lift $ doesDirectoryExist dir
        if exists
        then do
            contents <- lift $ getDirectoryContents dir
            let files = filter (\f -> takeExtension f == ".hs") contents
            transpileFiles outdir (map (dir </>) files )
        else error "No such directory"

-- | Transpile a list of files and put the transpiled files in a given output directory
transpileFiles :: FilePath -> [FilePath] -> ExceptT String IO ()
transpileFiles _ [] = return ()
transpileFiles outdir (file:toBeTrans) = do
    toBeTrans' <- transpileFile outdir toBeTrans file
    transpileFiles outdir toBeTrans'

-- | Transpile a single file by also checking their imports and transpiling them if necessary
transpileFile :: FilePath -> [FilePath] -> FilePath -> ExceptT String IO [FilePath]
transpileFile outdir toBeTrans file = do
    parseResult <- lift $ parseFile file
    case parseResult of
        f@ParseFailed{} -> error $ show f
        ParseOk m@(Module _ _mhead _pragmas imports _decls) -> do
            let importFiles = map (readImport (takeDirectory file)) imports
            (toBeTrans', env) <- checkImports outdir toBeTrans importFiles
            (ast', env) <- fromExcept (transform' (void m) env) 
            let result = prettyPrint ast'
            lift $ createDirectoryIfMissing True outdir
            lift $ writeFile (outdir </> takeFileName file) $ result ++ "\n"
            lift $ writeFile (outdir </> takeBaseName file <.> "icomp") $ show env 
            return toBeTrans'

-- | Read file name from an import declaration and return absoule path given a directory
readImport :: FilePath -> ImportDecl l -> FilePath
readImport dir (ImportDecl {importModule = ModuleName _ nam}) = dir </> nam <.> "hs"

-- | Check a list of imported files and returning their combined environment
-- while also transpiling them if no info file was found
checkImports :: FilePath -> [FilePath] -> [FilePath] -> ExceptT String IO ([FilePath], Env)
checkImports outdir toBeTrans [] = return (toBeTrans, emptyEnv)
checkImports outdir toBeTrans (file:files) = do
    (toBeTrans', env) <- getEnvFromInfoFile outdir toBeTrans file
    (toBeTrans'', env') <- checkImports outdir toBeTrans' files
    let 
        sig = Map.unionWith Set.union (fst env) (fst env')
        constrs = Set.union (snd env) (snd env')
    return (toBeTrans'', (sig, constrs))
        
-- | Return the environment for a file by checking its info file.
-- If not found, it transpiles the file first.
getEnvFromInfoFile :: FilePath -> [FilePath] -> FilePath -> ExceptT String IO ([FilePath], Env)
getEnvFromInfoFile outdir toBeTrans file = do
    let infoFile = outdir </> takeBaseName file <.> "icomp"
    existsInfo <- lift $ doesFileExist infoFile
    if existsInfo
        then lift $ do
                content <- readFile infoFile
                return $ (toBeTrans, read content)
        else do
            let existsModule = List.elem file toBeTrans
            if existsModule
               then do 
                   toBeTrans' <- transpileFile outdir toBeTrans file
                   getEnvFromInfoFile outdir (List.delete file toBeTrans') file
               else return (toBeTrans, emptyEnv)
               
-- | Wraps an Except to and Except transformer
fromExcept :: (Monad m) => Except e a -> ExceptT e m a
fromExcept = mapExceptT (return . runIdentity)
