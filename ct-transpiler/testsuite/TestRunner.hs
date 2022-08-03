module Main where

import TransformFiles

import Test.Tasty.Golden
import Test.Tasty.Options
import Test.Tasty
import Control.Monad.Except
import Data.Proxy
import System.FilePath
import System.Directory
import System.Process     (readProcessWithExitCode)
import System.Exit

type TestGroup = (FilePath, [FilePath])

-- | Perform transform and compile tests on provided testsuite files using Tasty. 
main :: IO ()
main = do
    clean `mapM_` validGroups
    transformTests <- lookupTestGroup `mapM` validGroups
    compileTests <- lookupTestGroup `mapM` ["good"]
    defaultMainWithIngredients ingredients $ askOption $ \(Compiler ghc) ->
        testGroup "Tests" [createTransformTree transformTests, createCompileTree ghc compileTests]
  where
    ingredients = includingOptions [Option (Proxy :: Proxy Compiler)] : defaultIngredients

newtype Compiler = Compiler String

instance IsOption Compiler where
  defaultValue = Compiler "ghc"
  parseValue = Just . Compiler
  optionName = return "compiler"
  optionHelp = return "The program used for test compilation"

-- | Clean directory of tests by removing generated "out" directory and ".out files"
-- If a test directory is empty after that, remove the directory as well.
clean :: FilePath -> IO ()
clean group = (cleanTest `mapM_`) =<< testDirs
  where
    mainDir = testsuite </> group
    testDirs :: IO [FilePath]
    testDirs = do
        exists <- doesDirectoryExist mainDir
        if exists
          then getDirectories mainDir
          else return []
    cleanTest :: FilePath -> IO ()
    cleanTest dir = do
        files <- (filter isOutFile) <$> getFiles dir
        mapM_ removeFile files
        let outDir = dir </> "out"
        existsOut <- doesDirectoryExist outDir
        if existsOut
           then removeDirectoryRecursive outDir
           else return()
        isEmpty <- (0 ==) . length <$> listDirectory dir
        if isEmpty
          then removeDirectory dir
          else return ()
    isOutFile path = takeExtension path == ".out"

lib :: FilePath
lib = "lib"

testsuite :: FilePath
testsuite = "testsuite"

groups :: [FilePath]
groups = ["good", "bad"]

validGroups :: [FilePath]
validGroups = ["good", "bad"]

lookupTestGroup :: FilePath -> IO TestGroup
lookupTestGroup group = do
    testDirs <- getTestDirs (testsuite </> group)
    return (group, testDirs)

-- | Get a list of all test directories that contain a Haskell file with the same name as the directory
getTestDirs :: FilePath -> IO [FilePath]
getTestDirs mainDir = do
    exists <- doesDirectoryExist mainDir
    if exists
      then do 
          dirs <- getDirectories mainDir
          filterM hasMainFile $ dirs
      else return []
    where hasMainFile = doesFileExist . toTestFile
          toTestFile dir = dir </> takeBaseName dir <.> "hs"

createTransformTree :: [TestGroup] -> TestTree
createTransformTree groups = testGroup "Transform tests" $ buildTestGroup buildTest <$> groups
  where buildTest = buildGoldenTest "Transform" runTransformTest

createCompileTree :: FilePath -> [TestGroup] -> TestTree
createCompileTree ghc groups = testGroup "Compile tests" $ buildTestGroup buildTest <$> groups
  where buildTest = buildGoldenTest "Compile" (runTransformAndCompileTest ghc)

buildTestGroup :: (FilePath -> TestTree) -> TestGroup -> TestTree
buildTestGroup testBuilder (group, dirs) = testGroup group $ testBuilder <$> dirs

buildGoldenTest :: String -> (FilePath -> FilePath -> IO ()) -> FilePath -> TestTree
buildGoldenTest testType execution dir = goldenVsFile name golden out (run dir out)
  where
    name = takeBaseName dir
    golden = dir </> name ++ testType <.> "golden"
    out = dir </> name ++ testType <.> "out"

-- | Performs transformation on all files in a directory, and write the result to a given output file
-- The result is either an error or the output for the main test file
runTransformTest :: FilePath -> FilePath -> IO ()
runTransformTest dir out = do
    let outdir = dir </> "out"
    res <- runExceptT (transpileFilesFromDir dir outdir)
    case res of
        Left msg ->  writeFileAndCreateDirectory out $ msg ++ "\n"
        Right _ -> do 
            let file = outdir </> takeBaseName dir <.> "hs"
            createDirectoryIfMissing True $ takeDirectory out
            copyFile file out            

runTransformAndCompileTest :: FilePath -> FilePath -> FilePath -> IO ()
runTransformAndCompileTest ghc dir out = do
    let outdir = dir </> "out"
        buildDir = dir </> "build"
        transformOutput = buildDir </> takeBaseName dir <.> "hs"
        transformOutputMain = outdir </> takeBaseName dir <.> "hs"
    runTransformTest dir transformOutput
    runCompileTest ghc buildDir transformOutputMain out
    removeDirectoryRecursive buildDir

-- | Compile the main test file in a directory and write either error or "OK" to a given output file
runCompileTest :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runCompileTest ghc buildDir file outFile = do
    createDirectoryIfMissing True buildDir
    let dir = takeDirectory file
    (exit,_out,err) <- readProcessWithExitCode ghc ["-i" ++ lib, "-i" ++ dir, "-outputdir", buildDir, file] []
    case exit of
         ExitSuccess -> writeFileAndCreateDirectory outFile $  "OK \n"
         ExitFailure _ ->  writeFileAndCreateDirectory outFile err

writeFileAndCreateDirectory :: FilePath -> String -> IO ()
writeFileAndCreateDirectory file text = do
    createDirectoryIfMissing True $ takeDirectory file
    writeBinaryFile file text

getFiles :: FilePath -> IO [FilePath]
getFiles directory = listDirectory directory
                      >>= return . map (directory </>)
                      >>= filterM doesFileExist

getDirectories :: FilePath -> IO [FilePath]
getDirectories directory = listDirectory directory
                      >>= return . map (directory </>)
                      >>= filterM doesDirectoryExist
