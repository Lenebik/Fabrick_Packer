module Output
  ( printLayout
  , printPiecesInfo
  , printError
  , writeReport
  ) where

import Types (Layout (..), PlacedPiece (..), Piece (..), AppError (..), Fabric (..))

-- ---------------------------------------------------------------------------
-- Layout output

printLayout :: String -> Layout -> IO ()
printLayout unit lay = do
  putStrLn $ "Ширина ткани  : " ++ showF (fabricWidth (layFabric lay)) ++ " " ++ unit
  putStrLn $ "Длина расклада: " ++ showF (layLength lay) ++ " " ++ unit
  putStrLn $ "Размещено лекал: " ++ show (length (layPlaced lay))
  mapM_ printPlaced (layPlaced lay)
  where
    printPlaced pp =
      putStrLn $ "  Лекало #" ++ show (pieceId (ppPiece pp))
               ++ "  позиция " ++ showPt (ppPosition pp)
               ++ "  угол " ++ show (round (ppRotation pp) :: Int) ++ "°"

-- ---------------------------------------------------------------------------
-- Pieces info (после парсинга)

printPiecesInfo :: String -> [Piece] -> IO ()
printPiecesInfo unit pieces = do
  putStrLn $ "Найдено лекал: " ++ show (length pieces)
  putStrLn (replicate 40 '-')
  mapM_ printPiece pieces
  putStrLn (replicate 40 '-')
  where
    printPiece p = do
      let poly    = piecePolygon p
          verts   = length poly
          area    = polygonArea poly
          (bw,bh) = boundingBox poly
      putStrLn $ "  Лекало #" ++ show (pieceId p)
               ++ " | вершин: " ++ show verts
               ++ " | площадь: " ++ showF area ++ " " ++ unit ++ "²"
               ++ " | размер: "  ++ showF bw ++ "×" ++ showF bh ++ " " ++ unit

-- | Площадь полигона по формуле Гаусса (шнурования)
polygonArea :: [(Double, Double)] -> Double
polygonArea []  = 0
polygonArea pts =
  let pairs = zip pts (drop 1 pts ++ take 1 pts)
      cross (( x1, y1), (x2, y2)) = x1 * y2 - x2 * y1
  in abs (sum (map cross pairs)) / 2

-- | Ширина и высота ограничивающего прямоугольника
boundingBox :: [(Double, Double)] -> (Double, Double)
boundingBox [] = (0, 0)
boundingBox pts =
  let w = maximum (map fst pts) - minimum (map fst pts)
      h = maximum (map snd pts) - minimum (map snd pts)
  in (w, h)

-- ---------------------------------------------------------------------------
-- Errors

printError :: AppError -> IO ()
printError = putStrLn . ("Ошибка: " ++) . describeError

describeError :: AppError -> String
describeError (ParseErr      msg) = "Парсинг: "     ++ msg
describeError (IOErr         msg) = "Файл: "        ++ msg
describeError (ValidationErr msg) = "Валидация: "   ++ msg
describeError (LayoutErr     msg) = "Расклад: "     ++ msg

-- ---------------------------------------------------------------------------
-- Report

writeReport :: FilePath -> [String] -> Layout -> IO ()
writeReport path logEntries lay = writeFile path (reportText logEntries lay)

reportText :: [String] -> Layout -> String
reportText logEntries lay = unlines $
  [ "=== Отчёт о раскладе ==="
  , "Ширина ткани  : " ++ showF (fabricWidth (layFabric lay)) ++ " см"
  , "Длина расклада: " ++ showF (layLength lay) ++ " см"
  , "Кол-во лекал  : " ++ show (length (layPlaced lay))
  , ""
  , "Размещение:"
  ] ++ map placedLine (layPlaced lay)
  ++ [ "", "=== Журнал действий и ошибок ===" ]
  ++ (if null logEntries then ["  (журнал пуст)"] else logEntries)
  where
    placedLine pp =
      "  Лекало #" ++ show (pieceId (ppPiece pp))
      ++ "  @ " ++ showPt (ppPosition pp)
      ++ "  " ++ show (round (ppRotation pp) :: Int) ++ "°"

-- ---------------------------------------------------------------------------
-- Helpers

showPt :: (Double, Double) -> String
showPt (x, y) = "(" ++ showF x ++ ", " ++ showF y ++ ")"

showF :: Double -> String
showF x = show (round x :: Int)
