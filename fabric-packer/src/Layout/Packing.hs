module Layout.Packing
  ( packPatterns
  ) where

import Types
import Layout.NFP (nfpVertices, polygonsOverlap)

import Data.List (sortBy, minimumBy)
import Data.Ord (comparing)

-- | Допустимые повороты лекал (градусы, кратные 90°).
allowedRotations :: [Angle]
allowedRotations = [0, 90, 180, 270]

-- ===========================================================================
-- Главная функция расклада
-- ===========================================================================

-- | NFP+BLF алгоритм:
-- 1. Лекала сортируются по убыванию площади.
-- 2. Для каждого лекала перебираются повороты.
-- 3. Для каждого поворота строятся кандидатные позиции — вершины NFP
--    относительно уже размещённых лекал плюс позиция (0,0) (угол ткани).
-- 4. Кандидаты фильтруются: попадают в ширину ткани, не перекрываются
--    с уже размещёнными.
-- 5. По правилу BLF (минимум Y, затем минимум X) выбирается лучшая позиция
--    среди всех валидных кандидатов и поворотов.
packPatterns :: Fabric -> [Piece] -> Layout
packPatterns fab pieces =
  let sorted = sortByAreaDesc pieces
      placed = foldl (placeOne fab) [] sorted
  in Layout fab placed (totalLength placed)

sortByAreaDesc :: [Piece] -> [Piece]
sortByAreaDesc = sortBy (comparing (negate . polygonArea . piecePolygon))

placeOne :: Fabric -> [PlacedPiece] -> Piece -> [PlacedPiece]
placeOne fab placed piece =
  case bestPlacement fab placed piece of
    Just pp -> placed ++ [pp]
    Nothing -> placed   -- не удалось разместить, пропускаем

-- ===========================================================================
-- Шаги 4-6: выбор лучшего поворота + позиции
-- ===========================================================================

bestPlacement :: Fabric -> [PlacedPiece] -> Piece -> Maybe PlacedPiece
bestPlacement fab placed piece =
  let attempts =
        [ (rot, pos)
        | rot <- allowedRotations
        , let poly = preparePoly rot (piecePolygon piece)
        , let (pw, _) = bboxSize poly
        , pw <= fabricWidth fab        -- лекало шире ткани → пропускаем поворот
        , pos <- maybeToList (bestBLFPosition fab placed poly pw)
        ]
  in case attempts of
       [] -> Nothing
       xs ->
         let (rot, pos) = minimumBy (comparing snd) xs    -- BLF: (y, x) — это и есть pos
         in Just (PlacedPiece piece pos rot)

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- | BLF: среди кандидатных позиций выбрать минимум по (y, x)
bestBLFPosition :: Fabric -> [PlacedPiece] -> Polygon -> Double -> Maybe Point
bestBLFPosition fab placed poly pw =
  let cands = candidatePositions fab placed poly pw
      valid = filter (positionFits fab placed poly) cands
  in case valid of
       [] -> Nothing
       _  -> Just (minimumBy (comparing (\(x, y) -> (y, x))) valid)

-- ===========================================================================
-- Генерация кандидатных позиций
-- ===========================================================================

-- | Кандидатные позиции для BLF:
-- 1. Вершины NFP относительно каждого размещённого лекала
-- 2. Те же вершины с x, прижатым к левому/правому краю ткани (пересечения
--    рёбер NFP с границами IFP — внутренней области ткани)
-- 3. Начало координат — угол ткани (всегда пробуем)
candidatePositions :: Fabric -> [PlacedPiece] -> Polygon -> Double -> [Point]
candidatePositions _   []     _    _  = [(0, 0)]
candidatePositions fab placed poly pw =
  let nfpCands = concatMap (\pp -> nfpVertices (placedPolygon pp) poly) placed
      maxX     = fabricWidth fab - pw
      edgeCands = concatMap (\(_, y) -> [(0, y), (maxX, y)]) nfpCands
  in nfpCands ++ edgeCands ++ [(0, 0)]

-- | Размещаемое лекало в координатах ткани
placedPolygon :: PlacedPiece -> Polygon
placedPolygon pp =
  translatePoly (ppPosition pp)
                (preparePoly (ppRotation pp) (piecePolygon (ppPiece pp)))

-- ===========================================================================
-- Проверка валидности позиции
-- ===========================================================================

positionFits :: Fabric -> [PlacedPiece] -> Polygon -> Point -> Bool
positionFits fab placed poly pos =
  let world = translatePoly pos poly
      withinFabric =
           minimum (map fst world) >= 0
        && maximum (map fst world) <= fabricWidth fab
        && minimum (map snd world) >= 0
      noOverlap = not (any (polygonsOverlap world . placedPolygon) placed)
  in withinFabric && noOverlap

-- ===========================================================================
-- Геометрические операции
-- ===========================================================================

preparePoly :: Angle -> Polygon -> Polygon
preparePoly rot pts = normalize (rotatePolygon rot pts)

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

translatePoly :: Point -> Polygon -> Polygon
translatePoly (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

bboxSize :: Polygon -> (Double, Double)
bboxSize pts =
  ( maximum (map fst pts) - minimum (map fst pts)
  , maximum (map snd pts) - minimum (map snd pts)
  )

polygonArea :: Polygon -> Double
polygonArea pts =
  abs (sum (zipWith cross pts (drop 1 pts ++ take 1 pts))) / 2
  where cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- | Итоговая длина расклада = максимум Y по всем размещённым полигонам
totalLength :: [PlacedPiece] -> Double
totalLength [] = 0
totalLength pps =
  maximum [ maximum (map snd (placedPolygon pp)) | pp <- pps ]
