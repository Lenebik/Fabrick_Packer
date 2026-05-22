{-# LANGUAGE TupleSections #-}
module Parse.Image
  ( loadPieces
  , scalePieces
  ) where

import Types (Piece (..), Polygon, Point, AppError (..))
import Codec.Picture (readImage, convertRGB8, imageWidth, imageHeight,
                      pixelAt, Image, PixelRGB8 (..))
import qualified Data.Set as Set
import Data.List (minimumBy, maximumBy, findIndex)
import Data.Ord (comparing)

-- ---------------------------------------------------------------------------
-- Tunables

minComponentSize :: Int
minComponentSize = 80     -- меньше — шум

darkThreshold :: Int
darkThreshold = 128       -- яркость < этого ⇒ чёрный пиксель

epsilonDP :: Double
epsilonDP = 2.0           -- допуск Douglas-Peucker (в пикселях)

-- ---------------------------------------------------------------------------
-- Public API

-- | Парсит изображение: возвращает лекала в пиксельных координатах и
-- ширину исходного изображения в пикселях (нужна для калибровки в см).
loadPieces :: FilePath -> IO (Either AppError ([Piece], Int))
loadPieces path = do
  result <- readImage path
  case result of
    Left err  -> return . Left . ParseErr $ "Не удалось открыть изображение: " ++ err
    Right img -> do
      let rgb    = convertRGB8 img
          w      = imageWidth  rgb
          h      = imageHeight rgb
          blacks = blackPixelSet rgb
          comps  = connectedComponents blacks
          large  = filter (\c -> Set.size c >= minComponentSize) comps
          pieces = zipWith (mkPiece h) [1..] large
          good   = filter (\p -> length (piecePolygon p) >= 3) pieces
      return (Right (good, w))

-- | Масштабирует все лекала единым коэффициентом (см на пиксель).
scalePieces :: Double -> [Piece] -> [Piece]
scalePieces s =
  map (\p -> p { piecePolygon = map (\(x, y) -> (x * s, y * s)) (piecePolygon p) })

mkPiece :: Int -> Int -> Set.Set (Int, Int) -> Piece
mkPiece imgH i comp =
  let traced  = mooreTrace comp
      pts     = map (toMathPoint imgH) traced
      simple  = douglasPeucker epsilonDP pts
      norm    = normalizeOrigin simple
  in Piece i norm

-- ---------------------------------------------------------------------------
-- Binarisation

blackPixelSet :: Image PixelRGB8 -> Set.Set (Int, Int)
blackPixelSet img = Set.fromList
  [ (x, y)
  | x <- [0 .. imageWidth  img - 1]
  , y <- [0 .. imageHeight img - 1]
  , isDark (pixelAt img x y)
  ]

isDark :: PixelRGB8 -> Bool
isDark (PixelRGB8 r g b) =
  (fromIntegral r + fromIntegral g + fromIntegral b :: Int) `div` 3 < darkThreshold

-- ---------------------------------------------------------------------------
-- Connected components (BFS, 8-связность)

connectedComponents :: Set.Set (Int, Int) -> [Set.Set (Int, Int)]
connectedComponents allPx = go allPx []
  where
    go remaining acc
      | Set.null remaining = acc
      | otherwise =
          let start      = Set.findMin remaining
              comp       = bfsComp allPx start
              remaining' = Set.difference remaining comp
          in go remaining' (comp : acc)

bfsComp :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
bfsComp pixels start = go [start] (Set.singleton start)
  where
    go []        visited = visited
    go (p:queue) visited =
      let newNs    = filter (`Set.notMember` visited)
                   $ filter (`Set.member`   pixels)
                   $ neighbors8 p
          visited' = foldr Set.insert visited newNs
      in go (queue ++ newNs) visited'

neighbors8 :: (Int, Int) -> [(Int, Int)]
neighbors8 (x, y) =
  [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]
                     , not (dx == 0 && dy == 0)]

-- ---------------------------------------------------------------------------
-- Moore-Neighbor boundary tracing

-- | 8 направлений по часовой стрелке, начиная с N. Координаты экрана (Y вниз).
dirs8 :: [(Int, Int)]
dirs8 =
  [ (0, -1)   -- 0  N
  , (1, -1)   -- 1  NE
  , (1,  0)   -- 2  E
  , (1,  1)   -- 3  SE
  , (0,  1)   -- 4  S
  , (-1, 1)   -- 5  SW
  , (-1, 0)   -- 6  W
  , (-1,-1)   -- 7  NW
  ]

dirIndex :: (Int, Int) -> (Int, Int) -> Int
dirIndex (px, py) (bx, by) =
  case findIndex (== (bx - px, by - py)) dirs8 of
    Just i  -> i
    Nothing -> 0    -- не должно произойти для соседей

addDir :: (Int, Int) -> (Int, Int) -> (Int, Int)
addDir (x, y) (dx, dy) = (x + dx, y + dy)

-- | Трассирует внешнюю границу компоненты, возвращает упорядоченный список
-- пикселей контура (один обход против часовой стрелки в экранных координатах,
-- т.е. по часовой в математических).
mooreTrace :: Set.Set (Int, Int) -> [(Int, Int)]
mooreTrace comp
  | Set.null comp = []
  | otherwise =
      let start  = minimumBy (comparing (\(x, y) -> (y, x))) (Set.toList comp)
          startB = (fst start - 1, snd start)   -- W соседа: гарантированно фон
      in start : trace start startB 0
  where
    maxSteps = Set.size comp * 8 + 64

    trace p b n
      | n >= maxSteps = []
      | otherwise =
          case findNext p b of
            Nothing             -> []
            Just (nextP, newB)
              | nextP == startP -> []
              | otherwise       -> nextP : trace nextP newB (n + 1)

    startP = minimumBy (comparing (\(x, y) -> (y, x))) (Set.toList comp)

    findNext p b = scan 1 b
      where
        bIdx = dirIndex p b
        scan i prevBg
          | i > 8     = Nothing
          | otherwise =
              let idx  = (bIdx + i) `mod` 8
                  cand = addDir p (dirs8 !! idx)
              in if Set.member cand comp
                   then Just (cand, prevBg)
                   else scan (i + 1) cand

-- ---------------------------------------------------------------------------
-- Coordinate transforms

-- | Пиксельные координаты (Y вниз) → математическая СК (Y вверх)
toMathPoint :: Int -> (Int, Int) -> Point
toMathPoint imgH (x, y) = (fromIntegral x, fromIntegral (imgH - 1 - y))

-- | Сдвиг полигона так, чтобы нижняя-левая точка стала началом координат
normalizeOrigin :: Polygon -> Polygon
normalizeOrigin [] = []
normalizeOrigin pts =
  let minX = minimum (map fst pts)
      minY = minimum (map snd pts)
  in map (\(x, y) -> (x - minX, y - minY)) pts

-- ---------------------------------------------------------------------------
-- Douglas-Peucker simplification

douglasPeucker :: Double -> [Point] -> [Point]
douglasPeucker _       []     = []
douglasPeucker _       [p]    = [p]
douglasPeucker _       [p, q] = [p, q]
douglasPeucker epsilon pts@(start:_) =
  let end               = last pts
      (maxDist, maxIdx) = maxPerpIndex start end pts
  in if maxDist <= epsilon
       then [start, end]
       else let left  = douglasPeucker epsilon (take (maxIdx + 1) pts)
                right = douglasPeucker epsilon (drop maxIdx pts)
            in case left of
                 [] -> right
                 _  -> init left ++ right

perpDist :: Point -> Point -> Point -> Double
perpDist (x1, y1) (x2, y2) (px, py) =
  let dx  = x2 - x1
      dy  = y2 - y1
      len = sqrt (dx * dx + dy * dy)
  in if len == 0
       then sqrt ((px - x1) * (px - x1) + (py - y1) * (py - y1))
       else abs ((py - y1) * dx - (px - x1) * dy) / len

maxPerpIndex :: Point -> Point -> [Point] -> (Double, Int)
maxPerpIndex start end pts =
  maximumBy (comparing fst)
  $ zip (map (perpDist start end) pts) [0..]
