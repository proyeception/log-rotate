{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forM_)
import Data.List (isPrefixOf, sortOn)
import Data.List.Split ()
import Data.Ord (Down(Down))
import Data.Semigroup ((<>))
import Data.String.Interpolate (i)
import Data.Time.Calendar.OrdinalDate (showOrdinalDate)
import Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import Data.Time.LocalTime (LocalTime(localDay, localTimeOfDay), ZonedTime(zonedTimeToLocalTime), getZonedTime)
import Options.Applicative
  ((<**>), auto, help, info, long, metavar, option, progDesc, short, strOption, execParser, helper, Parser)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, renameFile)

data RotateOptions = RotateOptions {
  maxFiles :: Int,
  logPath :: FilePath,
  oldLogsDirectory :: FilePath
} deriving (Show)

newtype Log = Log {
  index :: Int
} deriving (Show)

toFile :: Log -> FilePath
toFile Log {..} = [i|log.#{index}|]

age :: Log -> Log
age log = log { index = index log + 1 }

rotateParser :: Parser RotateOptions
rotateParser =
  RotateOptions
    <$> option auto (long "max-files" <> short 'm' <> metavar "MAX-FILES" <> help "Max number of logs to keep")
    <*> strOption (long "log-path" <> short 'p' <> metavar "PATH" <> help "Full path to the log file")
    <*> strOption (long "old-log-directory" <> short 'o' <> metavar "DIR" <> help "Folder in which to put the old files")

now :: IO String
now = do
  now <- getZonedTime
  let day  = show . localDay . zonedTimeToLocalTime $ now
  let time = show . localTimeOfDay . zonedTimeToLocalTime $ now
  return [i|#{day} #{take 8 time}|]


createLogDirectory :: FilePath -> IO ()
createLogDirectory = createDirectoryIfMissing True

rotateLog :: FilePath -> FilePath -> IO ()
rotateLog logPath logDirectory = do
  let destination = [i|#{logDirectory}/log.0|]
  renameFile logPath destination
  date <- now
  appendFile destination [i|End of log - #{date}\n|]

toLog :: String -> Log
toLog = Log . read . drop (length "log.")

expireLogs :: Int -> FilePath -> FilePath -> IO ()
expireLogs max logPath logDirectory = do
  (keep, delete) <- splitAt max . sortOn index . map toLog . filter ("log." `isPrefixOf`) <$> listDirectory logDirectory
  forM_ delete $ removeFile . absoluteLogPath
  forM_ (sortOn (Down . index) keep) $ \log -> renameFile (absoluteLogPath log) (absoluteLogPath . age $ log)
  where absoluteLogPath l = [i|#{logDirectory}/#{toFile l}|] :: String

main :: IO ()
main = do
  options@RotateOptions {..} <- execParser $ info (rotateParser <**> helper) (progDesc "Rotate logs")
  createLogDirectory oldLogsDirectory
  rotateLog logPath oldLogsDirectory
  expireLogs maxFiles logPath oldLogsDirectory
