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

transpileFiles :: FilePath -> [FilePath] -> ExceptT String IO ()
transpileFiles _ [] = return ()
transpileFiles outdir (file:toBeTrans) = do
    toBeTrans' <- transpileFile toBeTrans outdir file
    transpileFiles outdir toBeTrans'

transpileFile :: [FilePath] -> FilePath -> FilePath -> ExceptT String IO [FilePath]
transpileFile toBeTrans outdir file = do
    parseResult <- lift $ parseFile file
    case parseResult of
        f@ParseFailed{} -> error $ show f
        ParseOk m@(Module _ _mhead _pragmas imports _decls) -> do
            let importFiles = map (readImport (takeDirectory file)) imports
            (toBeTrans', env) <- transpileImports toBeTrans outdir importFiles
            (ast', env) <- fromExcept (transform' (void m) env) 
            let result = prettyPrint ast'
            lift $ createDirectoryIfMissing True outdir
            lift $ writeFile (outdir </> takeFileName file) $ result ++ "\n"
            lift $ writeFile (outdir </> takeBaseName file <.> "icomp") $ show env 
            return toBeTrans'

-- | Read file name from an import declaration and return absoule path given a directory
readImport :: FilePath -> ImportDecl l -> FilePath
readImport dir (ImportDecl {importModule = ModuleName _ nam}) = dir </> nam <.> "hs"

transpileImports :: [FilePath] -> FilePath -> [FilePath] -> ExceptT String IO ([FilePath], Env)
transpileImports toBeTrans outdir [] = return (toBeTrans, emptyEnv)
transpileImports toBeTrans outdir (file:files) = do
    (toBeTrans', env) <- getEnvFromInfoFile toBeTrans outdir file
    (toBeTrans'', env') <- transpileImports toBeTrans' outdir files
    let 
        sig = Map.unionWith Set.union (fst env) (fst env')
        constrs = Set.union (snd env) (snd env')
    return (toBeTrans'', (sig, constrs))
        
getEnvFromInfoFile :: [FilePath] -> FilePath -> FilePath -> ExceptT String IO ([FilePath], Env)
getEnvFromInfoFile toBeTrans outdir file = do
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
                   toBeTrans' <- transpileFile toBeTrans outdir file
                   getEnvFromInfoFile (List.delete file toBeTrans') outdir file
               else return (toBeTrans, emptyEnv)

fromExcept :: (Monad m) => Except e a -> ExceptT e m a
fromExcept = mapExceptT (return . runIdentity)
