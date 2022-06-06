module TransformFiles (transpileFile) where

import Language.Haskell.Exts

import Transform
import TransformUtils

import System.FilePath
import System.Directory

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.Except



transpileFile :: FilePath -> FilePath -> ExceptT String IO ()
transpileFile file out = do
    parseResult <- lift $ parseFile file
    case parseResult of
        f@ParseFailed{} -> error $ show f
        ParseOk ast -> do
            case runExcept (transformFile (void ast) file) of
                Left msg ->  error msg
                Right (ast', env) -> do 
                    let result = prettyPrint ast'
                    lift $ writeFileAndCreateDirectory (out <.> "hs") $ result ++ "\n"
                    lift $ writeFileAndCreateDirectory (out <.> "icomp") $ show env 
                    

transformFile :: Module () -> FilePath -> Except String (Module (), Env)
transformFile m@(Module _ _mhead _pragmas imports _decls) file = do
    let infoFiles = readImports imports
    transformTest m

readImports :: [ImportDecl ()] -> Except String Env
readImports = undefined
    

readImport :: ImportDecl () -> IO Env 
readImport (ImportDecl {importModule = ModuleName _ nam}) = do
    let infoFile = nam <.> ".icomp"
    exists <- doesFileExist infoFile
    if exists
        then do
                content <- readFile infoFile
                return $ read content
            else return (Map.empty, Set.empty)



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

-- do we want to have a specific file ending to our files to specifically transform those, 
-- or just be happy to transform everything else as id?


