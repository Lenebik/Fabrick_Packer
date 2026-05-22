{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.QuickCheck
import Data.Aeson (encode, decode)
import Data.List (tails)

import Types
import Layout.NFP (convexHull, polygonsOverlap)
import Layout.Packing (packPatterns)

-- ===========================================================================
-- Arbitrary instances
-- ===========================================================================

newtype RectPiece = RectPiece { unRectPiece :: Piece }
  deriving Show

instance Arbitrary RectPiece where
  arbitrary = do
    i <- arbitrary
    w <- choose (5.0, 50.0 :: Double)
    h <- choose (5.0, 50.0 :: Double)
    return $ RectPiece $ Piece (abs i + 1) [(0,0), (w,0), (w,h), (0,h)]

instance Arbitrary AppConfig where
  arbitrary = AppConfig
    <$> genName
    <*> genName
    <*> frequency [(1, return Nothing), (3, Just . getPositive <$> arbitrary)]
    where
      genName = listOf1 (elements (['a'..'z'] ++ ['0'..'9'] ++ "._-"))

-- ===========================================================================
-- Helper: placed polygon (mirrors Layout.Packing internals)
-- ===========================================================================

placedPolygon :: PlacedPiece -> Polygon
placedPolygon pp =
  let rotated    = rotatePoly (ppRotation pp) (piecePolygon (ppPiece pp))
      normed     = normPoly rotated
      (dx, dy)   = ppPosition pp
  in map (\(x,y) -> (x + dx, y + dy)) normed

rotatePoly :: Angle -> Polygon -> Polygon
rotatePoly r pts = case (round r :: Int) `mod` 360 of
  0   -> pts
  90  -> map (\(x,y) -> (-y,  x)) pts
  180 -> map (\(x,y) -> (-x, -y)) pts
  270 -> map (\(x,y) -> ( y, -x)) pts
  _   -> pts

normPoly :: Polygon -> Polygon
normPoly [] = []
normPoly pts =
  let mx = minimum (map fst pts)
      my = minimum (map snd pts)
  in map (\(x,y) -> (x - mx, y - my)) pts

-- ===========================================================================
-- Properties
-- ===========================================================================

-- 1. JSON encode/decode roundtrip для AppConfig
prop_configJsonRoundtrip :: AppConfig -> Bool
prop_configJsonRoundtrip cfg =
  decode (encode cfg) == Just cfg

-- 2. Все вершины выпуклой оболочки принадлежат входному множеству
prop_convexHullSubset :: [Point] -> Bool
prop_convexHullSubset pts =
  all (`elem` pts) (convexHull pts)

-- 3. Непустое входное множество даёт непустую оболочку
prop_convexHullNonEmpty :: NonEmptyList Point -> Bool
prop_convexHullNonEmpty (NonEmpty pts) =
  not (null (convexHull pts))

-- Генераторы для тестов пакинга
genFabW :: Gen Double
genFabW = choose (30.0, 300.0)

genPieces :: Int -> Gen [Piece]
genPieces maxN = do
  n <- choose (0, maxN)
  fmap (map unRectPiece) (vectorOf n arbitrary)

-- 4. Все размещённые лекала не выходят за ширину ткани
prop_packWithinWidth :: Property
prop_packWithinWidth =
  forAll genFabW $ \fabW ->
  forAll (genPieces 10) $ \pieces ->
  let layout = packPatterns (Fabric fabW) pieces
  in all (withinWidth fabW) (layPlaced layout)
  where
    withinWidth w pp =
      maximum (map fst (placedPolygon pp)) <= w + 1e-9

-- 5. Все координаты размещённых лекал неотрицательны
prop_packPositiveCoords :: Property
prop_packPositiveCoords =
  forAll genFabW $ \fabW ->
  forAll (genPieces 10) $ \pieces ->
  let layout = packPatterns (Fabric fabW) pieces
  in all positiveCoords (layPlaced layout)
  where
    positiveCoords pp =
      all (\(x,y) -> x >= -1e-9 && y >= -1e-9) (placedPolygon pp)

-- 6. Никакие два размещённых лекала не перекрываются
prop_packNoOverlap :: Property
prop_packNoOverlap =
  forAll genFabW $ \fabW ->
  forAll (genPieces 8) $ \pieces ->
  let placed = layPlaced (packPatterns (Fabric fabW) pieces)
      pairs  = [(a,b) | (a:rest) <- tails placed, b <- rest]
  in all (\(a,b) -> not (polygonsOverlap (placedPolygon a) (placedPolygon b))) pairs

-- ===========================================================================
-- Runner
-- ===========================================================================

check :: Testable p => String -> p -> IO ()
check name prop = do
  putStr (name ++ ": ")
  quickCheckWith stdArgs { maxSuccess = 300 } prop

main :: IO ()
main = do
  putStrLn "=== QuickCheck: fabric-packer ==="
  putStrLn ""
  check "JSON-roundtrip AppConfig             " prop_configJsonRoundtrip
  check "convexHull vertices ⊆ input          " prop_convexHullSubset
  check "convexHull non-empty on non-empty    " prop_convexHullNonEmpty
  check "placed pieces within fabric width    " prop_packWithinWidth
  check "placed pieces have non-negative coords" prop_packPositiveCoords
  check "no overlap between placed pieces     " prop_packNoOverlap
