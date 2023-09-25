import System.Environment (getArgs)
import System.FilePath ((</>))

import Control.Applicative ((*>))
import Control.Monad (liftM2)

import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)

trackedFiles :: FilePath -> ExceptT String IO [String]
trackedFiles repoPath = do
    (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell $ "git -C " ++ repoPath ++ " ls-files") ""
    case exitCode of
        ExitSuccess -> return (lines stdout)
        _           -> throwError stderr

displayFile :: (FilePath, String) -> String
displayFile (fileName, fileContent) = fileName ++ ":\n" ++
    (dropWhileEnd isSpace $ dropWhile isSpace fileContent)

mainWithExceptions :: ExceptT String IO ()
mainWithExceptions = do
    args <- liftIO getArgs
    repoPath <- case args of
        [repoPath] -> return repoPath
        _ -> throwError "Usage: a.out <repo-path>"
    
    repoContents <- (map (repoPath</>)) <$> trackedFiles repoPath
    fileContents <- foldr (liftM2 (:)) (return []) (map (liftIO . readFile) repoContents)
    (liftIO . putStrLn) $ intercalate "\n\n\n" (map displayFile $ zip repoContents fileContents)

main :: IO ()
main = do
    result <- runExceptT mainWithExceptions
    case result of
        Left e  -> putStrLn $ "Error: " ++ e
        Right _ -> return ()
