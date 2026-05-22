module Main (main) where

import Parse.Image (loadPieces)
import Output (printPiecesInfo)
import Types (Piece (..))

main :: IO ()
main = do
  result <- loadPieces "testimg.png"
  case result of
    Left err -> putStrLn ("FAIL: " ++ show err)
    Right (ps, _) -> do
      printPiecesInfo "пкс" ps
      putStrLn ""
      putStrLn "Первые 6 вершин каждого лекала:"
      mapM_ (\p -> putStrLn $ "  #" ++ show (pieceId p) ++ ": "
                            ++ show (take 6 (piecePolygon p))) ps
