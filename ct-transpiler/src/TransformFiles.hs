module TransformFiles (transpileFiles) where

import Language.Haskell.Exts

import Transform
import TransformUtils

import System.FilePath
import System.Directory

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except

transpileFiles :: FilePath -> FilePath -> ExceptT String IO ()
transpileFiles dir outdir = do
    exists <- lift $ doesDirectoryExist dir
    if exists
      then do
        contents <- lift $ getDirectoryContents dir
        let files = filter (\f -> takeExtension f == ".hs") contents
        transpileFiles' outdir (map (dir </>) files )
      else error "No such directory"
      
    where
        transpileFiles' :: FilePath -> [FilePath] -> ExceptT String IO ()
        transpileFiles' _ [] = return ()
        transpileFiles' outdir (file:toBeTrans) = do
            transpileFile toBeTrans outdir file
            transpileFiles' outdir toBeTrans 
            

transpileFile :: [FilePath] -> FilePath -> FilePath -> ExceptT String IO ()
transpileFile toBeTrans outdir file = do
    parseResult <- lift $ parseFile file
    case parseResult of
        f@ParseFailed{} -> error $ show f
        ParseOk m@(Module _ _mhead _pragmas imports _decls) -> do
            let importFiles = readImports imports
            res <- lift $ runExceptT $ transpileImports toBeTrans outdir importFiles
            case res of
                 Left msg -> error msg
                 Right (_, env) -> do
                     case runExcept (transform' (void m) env) of
                        Left msg ->  error msg
                        Right (ast', env) -> do 
                            let result = prettyPrint ast'
                            lift $ createDirectoryIfMissing True outdir
                            lift $ writeFile (outdir </> takeFileName file) $ result ++ "\n"
                            lift $ writeFile (outdir </> takeBaseName file <.> "icomp") $ show env 
            
    
    

readImports :: [ImportDecl l] -> [FilePath] 
readImports idecls = map readImport idecls
             
readImport :: ImportDecl l -> FilePath
readImport (ImportDecl {importModule = ModuleName _ nam}) = nam

transpileImports :: [FilePath] -> FilePath -> [FilePath] -> ExceptT String IO ([FilePath], Env)
transpileImports toBeTrans outdir [] = return (toBeTrans, (Map.empty, Set.empty))
transpileImports toBeTrans outdir (file:files) = do
    (toBeTrans', env) <- getEnvFromInfoFile toBeTrans outdir file
    (toBeTrans'', env') <- transpileImports toBeTrans' outdir files
    let 
        sig = Map.unionWith Set.union (fst env) (fst env')
        constrs = Set.union (snd env) (snd env')
    return (toBeTrans'', (sig, constrs))

        
getEnvFromInfoFile :: [FilePath] -> FilePath -> FilePath -> ExceptT String IO ([FilePath], Env)
getEnvFromInfoFile toBeTrans outdir nam = do
    let infoFile = outdir </> nam <.> "icomp"
    existsInfo <- lift $ doesFileExist infoFile
    if existsInfo
        then lift $ do
                content <- readFile infoFile
                return $ (toBeTrans, read content)
        else do
            let modFile = nam <.> "hs"
                existsModule = List.elem modFile (map takeFileName toBeTrans)
            if existsModule
               then do 
                   transpileFile toBeTrans outdir modFile
                   getEnvFromInfoFile (List.delete modFile toBeTrans) outdir nam
               else error $ "could not find info file nor file to be transpiled"
                 

