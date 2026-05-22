module Config
  ( defaultConfig
  , loadConfig
  , saveConfig
  ) where

import Types (AppConfig (..), AppError (..))
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesFileExist)

configPath :: FilePath
configPath = "fabric-packer.json"

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { cfgReportPath = "report.txt"
  , cfgLogFile    = "app.log"
  }

loadConfig :: IO (Either AppError AppConfig)
loadConfig = do
  exists <- doesFileExist configPath
  if not exists
    then return . Left $ IOErr ("Конфиг не найден: " ++ configPath)
    else do
      raw <- BL.readFile configPath
      case eitherDecode raw of
        Left err  -> return . Left  $ ParseErr ("Ошибка парсинга конфига: " ++ err)
        Right cfg -> return . Right $ cfg

saveConfig :: AppConfig -> IO ()
saveConfig cfg = BL.writeFile configPath (encode cfg)
