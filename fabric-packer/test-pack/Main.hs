module Main (main) where

import Parse.Image (loadPieces)
import Layout.Packing (packPatterns)
import Output (printPiecesInfo, printLayout)
import Types (Fabric (..), Layout (..), Piece (..))

import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
  args <- getArgs
  let fabW = case args of
        (w:_) -> read w
        []    -> 600.0
  putStrLn $ "Ширина ткани: " ++ show fabW ++ " пкс"
  putStrLn ""

  parseResult <- loadPieces "testimg.png"
  case parseResult of
    Left err -> putStrLn ("FAIL parse: " ++ show err)
    Right (ps, _) -> do
      printPiecesInfo "пкс" ps
      putStrLn ""
      putStrLn "Запуск NFP+BLF расклада..."
      t0 <- getCurrentTime
      let layout = packPatterns (Fabric fabW) ps
      t1 <- layout `seq` getCurrentTime
      putStrLn $ "Время: " ++ show (diffUTCTime t1 t0)
      putStrLn ""
      printLayout "пкс" layout
      putStrLn ""
      let total  = length ps
          placed = length (layPlaced layout)
      putStrLn $ "Размещено: " ++ show placed ++ "/" ++ show total
      putStrLn $ "Утилизация ткани (площадь лекал / площадь ткани): "
              ++ show (utilization layout ps) ++ "%"

utilization :: Layout -> [Piece] -> Int
utilization lay ps =
  let totalArea = sum (map (polygonArea . piecePolygon) ps)
      fabArea   = fabricWidth (layFabric lay) * layLength lay
  in if fabArea > 0
       then round (100 * totalArea / fabArea :: Double)
       else 0
  where
    polygonArea pts =
      abs (sum (zipWith cross pts (drop 1 pts ++ take 1 pts))) / 2
    cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
