module Input.CLI
  ( runCLI
  ) where

import Types
import Config (loadConfig, saveConfig)
import Output (printLayout, printPiecesInfo, printError, writeReport)
import Render (renderLayout)
import Parse.Image (loadPieces, scalePieces)
import Layout.Packing (packPatterns)
import Log (logEvent)

import System.IO (hFlush, stdout)

data AppState = AppState
  { stConfig      :: AppConfig
  , stPieces      :: [Piece]        -- в сантиметрах после калибровки
  , stPxPerCm     :: Maybe Double   -- разрешение для рендера PNG
  , stFabricWidth :: Maybe Double   -- ширина ткани, см
  , stLayout      :: Maybe Layout
  , stLog         :: [String]       -- журнал событий сессии (для отчёта)
  }

initialState :: AppConfig -> AppState
initialState cfg = AppState
  { stConfig      = cfg
  , stPieces      = []
  , stPxPerCm     = Nothing
  , stFabricWidth = Nothing
  , stLayout      = Nothing
  , stLog         = []
  }

runCLI :: AppConfig -> IO ()
runCLI cfg = loop (initialState cfg)

loop :: AppState -> IO ()
loop st = do
  showMenu
  action <- readUserAction
  st' <- recordEvent Info ("Выбрано действие: " ++ actionName action) st
  case action of
    Quit -> putStrLn "До свидания."
    _    -> runAction action st' >>= loop

-- ---------------------------------------------------------------------------
-- Логирование: в app.log и в журнал сессии (попадает в отчёт)

recordEvent :: LogLevel -> String -> AppState -> IO AppState
recordEvent lvl msg st = do
  entry <- logEvent (cfgLogFile (stConfig st)) lvl msg
  return st { stLog = stLog st ++ [entry] }

-- | Печатает ошибку пользователю и записывает её в журнал
recordError :: AppError -> AppState -> IO AppState
recordError err st = do
  printError err
  recordEvent Error (errMsg err) st

errMsg :: AppError -> String
errMsg (ParseErr      m) = "Ошибка парсинга: "  ++ m
errMsg (IOErr         m) = "Ошибка файла: "     ++ m
errMsg (ValidationErr m) = "Ошибка валидации: " ++ m
errMsg (LayoutErr     m) = "Ошибка расклада: "  ++ m

actionName :: UserAction -> String
actionName LoadImage      = "загрузка изображения лекал"
actionName SetFabricWidth = "задание ширины ткани"
actionName RunPacking     = "расчёт расклада"
actionName SaveReport     = "сохранение отчёта"
actionName SaveImage      = "сохранение изображения расклада"
actionName LoadConfig     = "загрузка конфига"
actionName SaveConfig     = "сохранение конфига"
actionName Quit           = "выход"

-- ---------------------------------------------------------------------------
-- Обработка действий

runAction :: UserAction -> AppState -> IO AppState
runAction LoadImage st = do
  path <- prompt "Путь к изображению лекал: "
  result <- loadPieces path
  case result of
    Left err -> recordError err st
    Right (pxPieces, pxW) -> do
      putStrLn $ "Ширина изображения: " ++ show pxW ++ " пкс"
      cmW <- promptDouble "Физическая ширина изображения (см): "
      if cmW <= 0
        then recordError (ValidationErr "Ширина изображения должна быть положительной") st
        else do
          let scale  = cmW / fromIntegral pxW   -- см на пиксель
              pieces = scalePieces scale pxPieces
          printPiecesInfo "см" pieces
          st' <- recordEvent Info
                   ("Загружено лекал: " ++ show (length pieces) ++ " из «" ++ path
                    ++ "», ширина изображения " ++ showCm cmW ++ " см") st
          return st' { stPieces  = pieces
                     , stPxPerCm = Just (fromIntegral pxW / cmW)
                     , stLayout  = Nothing }

runAction SetFabricWidth st = do
  w <- promptDouble "Ширина ткани (см): "
  if w <= 0
    then recordError (ValidationErr "Ширина ткани должна быть положительной") st
    else do
      st' <- recordEvent Info ("Ширина ткани задана: " ++ showCm w ++ " см") st
      return st' { stFabricWidth = Just w, stLayout = Nothing }

runAction RunPacking st =
  case (stPieces st, stFabricWidth st) of
    ([], _)      -> recordError (ValidationErr "Сначала загрузите изображение лекал") st
    (_, Nothing) -> recordError (ValidationErr "Укажите ширину ткани") st
    (ps, Just w) -> do
      let layout = packPatterns (Fabric w) ps
          placed = length (layPlaced layout)
          total  = length ps
      printLayout "см" layout
      st' <- recordEvent Info
               ("Расклад выполнен: размещено " ++ show placed ++ " из " ++ show total
                ++ " лекал, длина " ++ showCm (layLength layout) ++ " см") st
      st'' <- if placed < total
                then recordEvent Warning
                       ("Не размещено лекал: " ++ show (total - placed)
                        ++ " (не помещаются в ширину ткани)") st'
                else return st'
      return st'' { stLayout = Just layout }

runAction SaveReport st =
  case stLayout st of
    Nothing  -> recordError (ValidationErr "Сначала выполните расклад") st
    Just lay -> do
      let path = cfgReportPath (stConfig st)
      st' <- recordEvent Info ("Сохранение отчёта: " ++ path) st
      writeReport path (stLog st') lay
      putStrLn $ "Отчёт сохранён: " ++ path
      return st'

runAction SaveImage st =
  case stLayout st of
    Nothing  -> recordError (ValidationErr "Сначала выполните расклад") st
    Just lay -> do
      let pxPerCm = maybe 5 id (stPxPerCm st)   -- запасное разрешение 5 px/см
      renderLayout pxPerCm "layout.png" lay
      recordEvent Info "Изображение расклада сохранено: layout.png" st

runAction LoadConfig st = do
  result <- loadConfig
  case result of
    Left err  -> recordError err st
    Right cfg -> do
      let st' = st { stConfig = cfg, stFabricWidth = cfgFabricWidth cfg }
      let msg = case cfgFabricWidth cfg of
                  Nothing -> "Конфиг загружен (ширина ткани не задана)"
                  Just w  -> "Конфиг загружен, ширина ткани: " ++ showCm w ++ " см"
      putStrLn msg
      recordEvent Info msg st'

runAction SaveConfig st = do
  let cfg = (stConfig st) { cfgFabricWidth = stFabricWidth st }
  saveConfig cfg
  putStrLn "Конфиг сохранён."
  recordEvent Info ("Конфиг сохранён" ++ maybe "" (\w -> ", ширина ткани: " ++ showCm w ++ " см") (stFabricWidth st)) st

runAction Quit st = return st

showMenu :: IO ()
showMenu = do
  putStrLn ""
  putStrLn "=== Fabric Packer ==="
  putStrLn "  1. Загрузить изображение лекал"
  putStrLn "  2. Задать ширину ткани"
  putStrLn "  3. Рассчитать расклад"
  putStrLn "  4. Сохранить отчёт"
  putStrLn "  5. Сохранить изображение расклада (PNG)"
  putStrLn "  6. Загрузить конфиг"
  putStrLn "  7. Сохранить конфиг"
  putStrLn "  0. Выход"

readUserAction :: IO UserAction
readUserAction = do
  n <- promptInt "Выбор: "
  case n of
    1 -> return LoadImage
    2 -> return SetFabricWidth
    3 -> return RunPacking
    4 -> return SaveReport
    5 -> return SaveImage
    6 -> return LoadConfig
    7 -> return SaveConfig
    0 -> return Quit
    _ -> putStrLn "Неверный выбор, попробуйте снова." >> readUserAction

-- ---------------------------------------------------------------------------
-- Helpers

showCm :: Double -> String
showCm x = show (round x :: Int)

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

promptDouble :: String -> IO Double
promptDouble msg = do
  s <- prompt msg
  case reads s of
    [(v, "")] -> return v
    _         -> putStrLn "Введите число." >> promptDouble msg

promptInt :: String -> IO Int
promptInt msg = do
  s <- prompt msg
  case reads s of
    [(v, "")] -> return v
    _         -> putStrLn "Введите целое число." >> promptInt msg
