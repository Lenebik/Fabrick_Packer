module Input.FileLoader
  ( readImageFile
  , fileExists
  ) where

import Types (AppError (..))
import System.Directory (doesFileExist)
import qualified Data.ByteString as BS

readImageFile :: FilePath -> IO (Either AppError BS.ByteString)
readImageFile path = do
  exists <- doesFileExist path
  if not exists
    then return . Left . IOErr $ "Файл не найден: " ++ path
    else fmap Right (BS.readFile path)

fileExists :: FilePath -> IO Bool
fileExists = doesFileExist
