module Main (main) where

import Config (defaultConfig, loadConfig)
import Input.CLI (runCLI)

main :: IO ()
main = do
  cfg <- loadConfig >>= \result -> case result of
    Left _    -> return defaultConfig
    Right cfg -> return cfg
  runCLI cfg
