module Layout.NFP
  ( computeNFP
  , nfpVertices
  , polygonsOverlap
  , minkowskiSum
  , convexHull
  ) where

import Types (Polygon, Point)
import Data.List (minimumBy, sort)
import Data.Ord (comparing)

-- ===========================================================================
-- No-Fit Polygon
-- ===========================================================================

-- | NFP(A, B): траектория опорной точки B (её левого нижнего угла) при
-- скольжении B вокруг A без вращения. Любая позиция p ∈ NFP означает, что
-- B, помещённый в p, касается A, но не перекрывает её.
--
-- Для вогнутых полигонов используется выпуклая аппроксимация: NFP считается
-- как сумма Минковского выпуклых оболочек: ConvHull(A) ⊕ −ConvHull(B). Это
-- даёт корректный, но более консервативный NFP. Финальная проверка на
-- пересечение в Layout.Packing использует исходные (вогнутые) контуры.
computeNFP :: Polygon -> Polygon -> Polygon
computeNFP a b =
  let ah = convexHull a
      bh = map negPt (convexHull b)
  in minkowskiSum ah bh
  where
    negPt (x, y) = (-x, -y)

-- | Вершины NFP — кандидатные позиции для BLF.
nfpVertices :: Polygon -> Polygon -> [Point]
nfpVertices = computeNFP

-- ===========================================================================
-- Сумма Минковского двух выпуклых CCW-полигонов (O(n+m))
-- ===========================================================================

minkowskiSum :: Polygon -> Polygon -> Polygon
minkowskiSum p q = case (rotateToBottommost (orientCCW p), rotateToBottommost (orientCCW q)) of
  (p0:_pTail, q0:_qTail) | length p >= 2 && length q >= 2 ->
    let edsP  = polyEdges (p0 : _pTail)
        edsQ  = polyEdges (q0 : _qTail)
        eds   = mergeByAngle edsP edsQ
        start = p0 `addP` q0
    in scanl addP start (init eds)
  _ -> []

addP :: Point -> Point -> Point
addP (a, b) (c, d) = (a + c, b + d)

-- | Векторы рёбер замкнутого полигона (vᵢ₊₁ − vᵢ)
polyEdges :: Polygon -> [Point]
polyEdges pts =
  zipWith sub (drop 1 pts ++ take 1 pts) pts
  where
    sub (a, b) (c, d) = (a - c, b - d)

-- | Слияние двух последовательностей рёбер по полярному углу
mergeByAngle :: [Point] -> [Point] -> [Point]
mergeByAngle [] qs = qs
mergeByAngle ps [] = ps
mergeByAngle (p:ps) (q:qs)
  | edgeAngle p <= edgeAngle q = p : mergeByAngle ps (q:qs)
  | otherwise                  = q : mergeByAngle (p:ps) qs

edgeAngle :: Point -> Double
edgeAngle (x, y) = atan2 y x

-- | Сдвигает вершины так, чтобы первой была самая нижняя-левая
rotateToBottommost :: Polygon -> Polygon
rotateToBottommost pts =
  let i = fst $ minimumBy (comparing (\(_, (x, y)) -> (y, x))) (zip [0..] pts)
  in drop i pts ++ take i pts

-- ===========================================================================
-- Ориентация и выпуклая оболочка
-- ===========================================================================

signedArea :: Polygon -> Double
signedArea pts = sum (zipWith cr pts (drop 1 pts ++ take 1 pts)) / 2
  where cr (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

orientCCW :: Polygon -> Polygon
orientCCW pts = if signedArea pts >= 0 then pts else reverse pts

-- | Алгоритм Эндрю (monotone chain), O(n log n)
convexHull :: [Point] -> Polygon
convexHull pts
  | length pts < 3 = pts
  | otherwise =
      let sorted = sort pts
          lower  = build sorted
          upper  = build (reverse sorted)
      in init lower ++ init upper
  where
    build = foldl step []
    step (b:a:rest) p
      | cross a b p <= 0 = step (a:rest) p
      | otherwise        = p : b : a : rest
    step acc p = p : acc
    cross (ox, oy) (ax, ay) (bx, by) =
      (ax - ox) * (by - oy) - (ay - oy) * (bx - ox)

-- ===========================================================================
-- Проверка пересечения двух (возможно вогнутых) полигонов
-- ===========================================================================

polygonsOverlap :: Polygon -> Polygon -> Bool
polygonsOverlap a b
  | not (bboxOverlap (bbox a) (bbox b)) = False
  | any (`pointInPolygon` b) a          = True
  | any (`pointInPolygon` a) b          = True
  | otherwise                            = anyEdgesIntersect a b

type BBox = (Double, Double, Double, Double)

bbox :: Polygon -> BBox
bbox pts = (minimum xs, minimum ys, maximum xs, maximum ys)
  where xs = map fst pts; ys = map snd pts

bboxOverlap :: BBox -> BBox -> Bool
bboxOverlap (a1, b1, c1, d1) (a2, b2, c2, d2) =
  not (c1 < a2 || c2 < a1 || d1 < b2 || d2 < b1)

-- | Метод трассировки луча (ray-casting): точка внутри, если число пересечений нечётно
pointInPolygon :: Point -> Polygon -> Bool
pointInPolygon (px, py) poly = foldr step False (edges poly)
  where
    edges pts = zip pts (drop 1 pts ++ take 1 pts)
    step ((x1, y1), (x2, y2)) inside
      | (y1 > py) /= (y2 > py)
        && px < (x2 - x1) * (py - y1) / (y2 - y1) + x1
        = not inside
      | otherwise = inside

anyEdgesIntersect :: Polygon -> Polygon -> Bool
anyEdgesIntersect a b = or
  [ segmentsIntersect p1 p2 q1 q2
  | (p1, p2) <- edges a
  , (q1, q2) <- edges b
  ]
  where edges pts = zip pts (drop 1 pts ++ take 1 pts)

-- | Строгое пересечение отрезков (касание не считается пересечением)
segmentsIntersect :: Point -> Point -> Point -> Point -> Bool
segmentsIntersect p1 p2 q1 q2 =
  let d1 = orient q1 q2 p1
      d2 = orient q1 q2 p2
      d3 = orient p1 p2 q1
      d4 = orient p1 p2 q2
  in d1 /= d2 && d3 /= d4
  where
    orient :: Point -> Point -> Point -> Int
    orient (ax, ay) (bx, by) (cx, cy) =
      let v = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
      in if v > 0 then 1 else if v < 0 then -1 else 0
