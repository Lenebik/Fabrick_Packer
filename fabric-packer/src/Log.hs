module Log
  ( initLogging
  , logEvent
  , formatEntry
  ) where

import Types (LogLevel (..))
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

initLogging :: FilePath -> IO ()
initLogging _ = return ()

-- | Пишет событие в файл лога и возвращает отформатированную строку
-- (используется для накопления журнала сессии в отчёте).
logEvent :: FilePath -> LogLevel -> String -> IO String
logEvent path level msg = do
  entry <- formatEntry level msg
  appendFile path (entry ++ "\n")
  return entry

formatEntry :: LogLevel -> String -> IO String
formatEntry level msg = do
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  return $ "[" ++ ts ++ "] [" ++ show level ++ "] " ++ msg
