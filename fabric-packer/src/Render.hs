module Render
  ( renderLayout
  ) where

import Types (Layout (..), PlacedPiece (..), Piece (..), Fabric (..),
              Polygon, Angle)
import Codec.Picture (PixelRGB8 (..), generateImage, writePng)
import qualified Data.Set as Set

-- | Рендерит расклад в PNG: белый фон, чёрные контуры лекал.
-- Лекала заданы в см; @pxPerCm@ — разрешение вывода (пикселей на см).
renderLayout :: Double -> FilePath -> Layout -> IO ()
renderLayout pxPerCm path lay
  | null (layPlaced lay) = putStrLn "Нечего рендерить: расклад пуст"
  | otherwise = do
      let w = max 1 (ceiling (fabricWidth (layFabric lay) * pxPerCm))
          h = max 1 (ceiling (layLength lay * pxPerCm))
          edgePixels = Set.fromList
            [ (x, h - 1 - y)        -- математические Y → экранные (Y вниз)
            | pp <- layPlaced lay
            , let poly = map (\(px, py) -> (px * pxPerCm, py * pxPerCm))
                             (placedWorldPolygon pp)
            , (x, y) <- polygonOutlinePixels poly
            , x >= 0, x < w, y >= 0, y < h
            ]
          img = generateImage (\x y ->
                  if Set.member (x, y) edgePixels
                    then PixelRGB8   0   0   0
                    else PixelRGB8 255 255 255) w h
      writePng path img
      putStrLn $ "Изображение сохранено: " ++ path
                 ++ " (" ++ show w ++ "×" ++ show h ++ " пкс)"

-- ===========================================================================
-- Геометрические преобразования (дублируют Layout.Packing, чтобы не плодить
-- внутренние зависимости — Render используется отдельно для визуализации).
-- ===========================================================================

placedWorldPolygon :: PlacedPiece -> Polygon
placedWorldPolygon pp =
  let rotated  = rotatePolygon (ppRotation pp) (piecePolygon (ppPiece pp))
      norm     = normalize rotated
      (dx, dy) = ppPosition pp
  in map (\(x, y) -> (x + dx, y + dy)) norm

rotatePolygon :: Angle -> Polygon -> Polygon
rotatePolygon r pts = case round r `mod` 360 :: Int of
  0   -> pts
  90  -> map (\(x, y) -> (-y,  x)) pts
  180 -> map (\(x, y) -> (-x, -y)) pts
  270 -> map (\(x, y) -> ( y, -x)) pts
  _   -> pts

normalize :: Polygon -> Polygon
normalize pts =
  let mx = minimum (map fst pts)
      my = minimum (map snd pts)
  in map (\(x, y) -> (x - mx, y - my)) pts

-- ===========================================================================
-- Растеризация контура полигона (Bresenham по каждому ребру)
-- ===========================================================================

polygonOutlinePixels :: Polygon -> [(Int, Int)]
polygonOutlinePixels [] = []
polygonOutlinePixels pts =
  concatMap (uncurry bresenham) edges
  where
    edges = zip (map toPx pts) (map toPx (drop 1 pts ++ take 1 pts))
    toPx (x, y) = (round x, round y)

-- | Алгоритм Брезенхема для отрезка между двумя пикселями
bresenham :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bresenham (x0, y0) (x1, y1) = go x0 y0 (dx - dy)
  where
    dx = abs (x1 - x0)
    dy = abs (y1 - y0)
    sx = if x0 < x1 then 1 else -1
    sy = if y0 < y1 then 1 else -1
    go x y err
      | x == x1 && y == y1 = [(x, y)]
      | otherwise =
          let e2 = 2 * err
              (x', errX) = if e2 > (- dy) then (x + sx, err - dy) else (x, err)
              (y', errY) = if e2 <   dx  then (y + sy, errX + dx) else (y, errX)
          in (x, y) : go x' y' errY
