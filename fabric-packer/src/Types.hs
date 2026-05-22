{-# LANGUAGE DeriveGeneric #-}
module Types
  ( Point
  , Polygon
  , Angle
  , Piece (..)
  , Fabric (..)
  , PlacedPiece (..)
  , Layout (..)
  , AppConfig (..)
  , AppError (..)
  , LogLevel (..)
  , UserAction (..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Geometry

type Point = (Double, Double)
type Polygon = [Point]
type Angle = Double   -- degrees: 0, 90, 180, 270

-- | Лекало — порядковый номер и замкнутый полигон (начало СК в нижней-левой точке)
data Piece = Piece
  { pieceId      :: Int
  , piecePolygon :: Polygon
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Layout

-- | Ткань: прямоугольник заданной ширины, длина вычисляется при расчёте расклада
data Fabric = Fabric
  { fabricWidth :: Double  -- ширина, см
  } deriving (Show, Eq)

data PlacedPiece = PlacedPiece
  { ppPiece    :: Piece
  , ppPosition :: Point   -- нижний-левый угол в СК ткани
  , ppRotation :: Angle
  } deriving (Show, Eq)

data Layout = Layout
  { layFabric :: Fabric
  , layPlaced :: [PlacedPiece]
  , layLength :: Double   -- итоговая длина расклада, см
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Config & errors

data AppConfig = AppConfig
  { cfgReportPath  :: FilePath      -- путь для сохранения отчёта
  , cfgLogFile     :: FilePath      -- путь к файлу логов
  , cfgFabricWidth :: Maybe Double  -- ширина ткани по умолчанию, см
  } deriving (Show, Eq, Generic)

instance FromJSON AppConfig
instance ToJSON   AppConfig

data AppError
  = ParseErr      String
  | IOErr         String
  | ValidationErr String
  | LayoutErr     String
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Logging & CLI

data LogLevel = Info | Warning | Error
  deriving (Show, Eq, Ord)

data UserAction
  = LoadImage
  | SetFabricWidth
  | RunPacking
  | SaveReport
  | SaveImage
  | LoadConfig
  | SaveConfig
  | Quit
  deriving (Show, Eq)
