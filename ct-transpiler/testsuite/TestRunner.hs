module Main where

import TransformFiles

import qualified Test.Tasty        as Tasty
import qualified Test.Tasty.Golden as Gold
import Test.Tasty         (TestTree)
import Test.Tasty.Options (IsOption(..), OptionDescription(Option))

import Control.Monad.Except (runExceptT, filterM)
import Data.Proxy (Proxy(Proxy))

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import System.FilePath ((</>), (<.>))
import System.Exit (ExitCode(..))
import System.Process     (readProcessWithExitCode)

type TestGroup = (FilePath, [FilePath])

-- | Perform transform and compile tests on provided testsuite files using Tasty. 
main :: IO ()
main = do
    clean `mapM_` validGroups
    transformTests <- lookupTestGroup `mapM` validGroups
    compileTests <- lookupTestGroup `mapM` ["good"]
    Tasty.defaultMainWithIngredients ingredients $ Tasty.askOption $ \(Compiler ghc) ->
        Tasty.testGroup "Tests" [createTransformTree transformTests, createCompileTree ghc compileTests]
  where
    ingredients = Tasty.includingOptions [Option (Proxy :: Proxy Compiler)] : Tasty.defaultIngredients

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
        exists <- Dir.doesDirectoryExist mainDir
        if exists
          then getDirectories mainDir
          else return []
    cleanTest :: FilePath -> IO ()
    cleanTest dir = do
        files <- (filter isOutFile) <$> getFiles dir
        mapM_ Dir.removeFile files
        let outDir = dir </> "out"
        existsOut <- Dir.doesDirectoryExist outDir
        if existsOut
           then Dir.removeDirectoryRecursive outDir
           else return()
        isEmpty <- (0 ==) . length <$> Dir.listDirectory dir
        if isEmpty
          then Dir.removeDirectory dir
          else return ()
    isOutFile path = Path.takeExtension path == ".out"

lib :: FilePath
lib = "lib"

testsuite :: FilePath
testsuite = "testsuite"

validGroups :: [FilePath]
validGroups = ["good", "bad"]

lookupTestGroup :: FilePath -> IO TestGroup
lookupTestGroup group = do
    testDirs <- getTestDirs (testsuite </> group)
    return (group, testDirs)

-- | Get a list of all test directories that contain a Haskell file with the same name as the directory
getTestDirs :: FilePath -> IO [FilePath]
getTestDirs mainDir = do
    exists <- Dir.doesDirectoryExist mainDir
    if exists
      then do 
          dirs <- getDirectories mainDir
          filterM hasMainFile $ dirs
      else return []
    where hasMainFile = Dir.doesFileExist . toTestFile
          toTestFile dir = dir </> Path.takeBaseName dir <.> "hs"

createTransformTree :: [TestGroup] -> TestTree
createTransformTree groups = Tasty.testGroup "Transform tests" $ buildTestGroup buildTest <$> groups
  where buildTest = buildGoldenTest "Transform" runTransformTest

createCompileTree :: FilePath -> [TestGroup] -> TestTree
createCompileTree ghc groups = Tasty.testGroup "Compile tests" $ buildTestGroup buildTest <$> groups
  where buildTest = buildGoldenTest "Compile" (runTransformAndCompileTest ghc)

buildTestGroup :: (FilePath -> TestTree) -> TestGroup -> TestTree
buildTestGroup testBuilder (group, dirs) = Tasty.testGroup group $ testBuilder <$> dirs

buildGoldenTest :: String -> (FilePath -> FilePath -> IO ()) -> FilePath -> TestTree
buildGoldenTest testType execution dir = Gold.goldenVsFile name golden out (execution dir out)
  where
    name = Path.takeBaseName dir
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
            let file = outdir </> Path.takeBaseName dir <.> "hs"
            result <- readFile file
            writeFileAndCreateDirectory out $ result

runTransformAndCompileTest :: FilePath -> FilePath -> FilePath -> IO ()
runTransformAndCompileTest ghc dir out = do
    let outdir = dir </> "out"
        buildDir = dir </> "build"
        transformOutput = buildDir </> Path.takeBaseName dir <.> "hs"
        transformOutputMain = outdir </> Path.takeBaseName dir <.> "hs"
    runTransformTest dir transformOutput
    runCompileTest ghc buildDir transformOutputMain out
    Dir.removeDirectoryRecursive buildDir

-- | Compile the main test file in a directory and write either error or "OK" to a given output file
runCompileTest :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runCompileTest ghc buildDir file outFile = do
    Dir.createDirectoryIfMissing True buildDir
    let dir = Path.takeDirectory file
    (exit,_out,err) <- readProcessWithExitCode ghc ["-i" ++ lib, "-i" ++ dir, "-outputdir", buildDir, file, "-fno-code"] []
    case exit of
         ExitSuccess -> writeFileAndCreateDirectory outFile $  "OK \n"
         ExitFailure _ ->  writeFileAndCreateDirectory outFile err

writeFileAndCreateDirectory :: FilePath -> String -> IO ()
writeFileAndCreateDirectory file text = do
    Dir.createDirectoryIfMissing True $ Path.takeDirectory file
    Gold.writeBinaryFile file text

getFiles :: FilePath -> IO [FilePath]
getFiles directory = Dir.listDirectory directory
                      >>= return . map (directory </>)
                      >>= filterM Dir.doesFileExist

getDirectories :: FilePath -> IO [FilePath]
getDirectories directory = Dir.listDirectory directory
                      >>= return . map (directory </>)
                      >>= filterM Dir.doesDirectoryExist
