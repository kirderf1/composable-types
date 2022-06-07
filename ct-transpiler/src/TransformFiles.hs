module TransformFiles (transpileFiles) where

import Language.Haskell.Exts

import Transform
import TransformUtils

import System.FilePath
import System.Directory

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
        mapM_ (transpileFile outdir) (map (dir </>) files )
      else error "No such directory"

transpileFile :: FilePath -> FilePath -> ExceptT String IO ()
transpileFile outdir file = do
    parseResult <- lift $ parseFile file
    case parseResult of
        f@ParseFailed{} -> error $ show f
        ParseOk m@(Module _ _mhead _pragmas imports _decls) -> do
            case (runExceptT (readImports imports outdir)) of
                 Left file' -> transpileFile outdir file'
                 Right env -> do
                     case runExcept (transform' (void m) env) of
                        Left msg ->  error msg
                        Right (ast', env) -> do 
                            let result = prettyPrint ast'
                            lift $ createDirectoryIfMissing True outdir
                            lift $ writeFile (outdir </> takeFileName file) $ result ++ "\n"
                            lift $ writeFile (outdir </> takeBaseName file <.> "icomp") $ show env 
            
    
    

readImports :: [ImportDecl l] -> FilePath -> ExceptT String IO Env
readImports idecls outdir = do
    envs <- mapM (readImport outdir) idecls
    let (sigs, constrs) = unzip envs
        sig = foldr (Map.unionWith Set.union) Map.empty sigs
        constrs' = foldr Set.union Set.empty constrs
    return (sig, constrs')
    

readImport :: FilePath -> ImportDecl l -> ExceptT String IO Env 
readImport outdir (ImportDecl {importModule = ModuleName _ nam}) = do
    let infoFile = outdir </> nam <.> ".icomp"
    exists <- lift $ doesFileExist infoFile
    if exists
        then lift $ do
                content <- readFile infoFile
                return $ read content
        else error $ nam



writeFileAndCreateDirectory :: FilePath -> String -> IO ()
writeFileAndCreateDirectory file text = do
    createDirectoryIfMissing True $ takeDirectory file
    writeFile file text







-- parseFile
-- transform file:
-- if it imports:
    -- transform file with use of info files from imports
    -- if no info files (or no transformed files?), transform the imported file itself if it exists
-- when transforming, make sure to create info file
-- prettyprint result (return string, or always write to a file?)
-- probably take in another argument for where to put info file and transformed file

-- info file:
-- save environment as (Sig, Constrs) which contains categories, pieces and piece constructors

-- transform (module) should return the transformed module and (Sig, Constrs) 
-- so that we can create info file and transformed file from that


