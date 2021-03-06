module Main where

import Language.Haskell.Exts
import Transform
import TransformFiles

import Test.Tasty.Golden
import Test.Tasty
import Control.Monad.Except
import System.FilePath
import System.Directory
import System.Process     (readProcessWithExitCode)
import System.Exit

-- | Perform transform and compile tests on provided testsuite files using Tasty. 
main :: IO ()
main = do
    clean `mapM_` groups
    transformTests <- createTransformTree `mapM` groups
    compileTests <- createCompileTree `mapM` ["good"]
    defaultMain $ testGroup "Tests" 
        [(testGroup "Transform tests") transformTests, 
         (testGroup "Compile tests") compileTests]

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

createTransformTree :: FilePath -> IO TestTree
createTransformTree group = testGolden group "Transform" runTransformTest <$> getTestDirs (testsuite </> group)

createCompileTree :: FilePath -> IO (TestTree)
createCompileTree group = testGolden group "Compile" runTransformAndCompileTest <$> getTestDirs (testsuite </> group)

-- | Perform a given test on a group of test files and compare result with golden file
testGolden :: String -> String -> (FilePath -> FilePath -> IO ()) -> [FilePath] -> TestTree
testGolden group test run dirs = testGroup group $ do
    dir <- dirs
    let golden = dir </> takeBaseName dir ++ test <.> "golden"
        out = dir </> takeBaseName dir ++ test <.> "out"
    return $ goldenVsFile (takeBaseName dir) golden out (run dir out)

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

runTransformAndCompileTest :: FilePath -> FilePath -> IO ()
runTransformAndCompileTest dir out = do
    let outdir = dir </> "out"
        buildDir = dir </> "build"
        transformOutput = buildDir </> takeBaseName dir <.> "hs"
        transformOutputMain = outdir </> takeBaseName dir <.> "hs"
    runTransformTest dir transformOutput
    runCompileTest buildDir transformOutputMain out
    removeDirectoryRecursive buildDir
                  
-- | Compile the main test file in a directory and write either error or "OK" to a given output file
runCompileTest :: FilePath -> FilePath -> FilePath -> IO ()
runCompileTest buildDir file outFile = do
    createDirectoryIfMissing True buildDir
    let dir = takeDirectory file
    (exit,_out,err) <- readProcessWithExitCode "ghc" ["-i" ++ lib, "-i" ++ dir, "-outputdir", buildDir, file] []
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
