{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.FilePath ((</>))

import Control.Applicative ((*>))
import Control.Monad (liftM2, when)
import Control.Monad.State (State, evalState, put, get)

import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, filter)
import Text.Regex.TDFA ((=~))

import Options.Applicative (Parser, argument, metavar, help, option, short, value, str, execParser, info, helper, fullDesc, progDesc, (<**>))

data Args = Args {
    repoPath :: FilePath,
    regexPattern :: String,
    inverseRegexPattern :: String
}

argsParser :: Parser Args
argsParser = Args
    <$> argument str (metavar "REPO_PATH" <> help "Path to the repository")
    <*> option str (short 'r' <> metavar "REGEX" <> value "" <> help "Regex pattern to filter the files")
    <*> option str (short 'v' <> metavar "INV_REGEX" <> value "" <> help "Regex pattern to filter out the files")

trackedFiles :: FilePath -> ExceptT String IO [FilePath]
trackedFiles repoPath = do
    (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell $ "git -C " ++ repoPath ++ " ls-files") ""
    case exitCode of
        ExitSuccess -> return (lines stdout)
        _           -> throwError stderr

displayFile :: (FilePath, String) -> String
displayFile (fileName, fileContent) = fileName ++ ":\n" ++
    (dropWhileEnd isSpace $ dropWhile isSpace fileContent)

filterRepoContents :: String -> String -> State [FilePath] [FilePath]
filterRepoContents regexPattern inverseRegexPattern = do
    repoContents <- get
    when (not $ null regexPattern) $ put (filter (=~ regexPattern) repoContents)

    repoContents <- get
    when (not $ null inverseRegexPattern) $ put (filter (not . (=~ inverseRegexPattern)) repoContents)

    get >>= return

mainWithExceptions :: Args -> ExceptT String IO ()
mainWithExceptions args = do
    let repoPath_ = repoPath args

    repoContents <- (map (repoPath_ </>)) <$> trackedFiles repoPath_
    let filteredRepoContents = evalState (filterRepoContents (regexPattern args) (inverseRegexPattern args)) repoContents
    fileContents <- liftIO $ foldr (liftM2 (:)) (return []) (map readFile filteredRepoContents)
    (liftIO . putStrLn) $ intercalate "\n\n\n" (map displayFile $ zip filteredRepoContents fileContents)

main :: IO ()
main = do
    args <- execParser $ info (argsParser <**> helper) (fullDesc <> progDesc "Display content of files in a repository")
    result <- runExceptT $ mainWithExceptions args
    case result of
        Left e  -> putStrLn $ "Error: " ++ e
        Right _ -> return ()
