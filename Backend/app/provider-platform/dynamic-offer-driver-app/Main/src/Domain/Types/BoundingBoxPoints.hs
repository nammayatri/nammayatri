{-# OPTIONS_GHC -Wno-orphans #-}

-- | Bounding box stored as JSON array of lat-long points (consecutive segments = sides).
-- Used for geometry table bbox column; aligns with Kernel.Utils.ComputeIntersection.BoundingBox.
-- Stored as jsonb in DB, same pattern as DocStatus in DigilockerVerification.
module Domain.Types.BoundingBoxPoints where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude

-- | Array of points where consecutive segments denote a side (e.g. 4 points = rectangle).
newtype BoundingBoxPoints = BoundingBoxPoints {unBoundingBoxPoints :: [LatLong]}
  deriving (Show, Eq, Ord, Generic)

instance ToJSON BoundingBoxPoints where
  toJSON (BoundingBoxPoints pts) = A.toJSON pts

instance FromJSON BoundingBoxPoints where
  parseJSON v = BoundingBoxPoints <$> A.parseJSON v

-- Database integration (same pattern as Domain.Types.DocStatus)
fromFieldBoundingBoxPoints ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion BoundingBoxPoints
fromFieldBoundingBoxPoints f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "BoundingBoxPoints: conversion failed"

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be BoundingBoxPoints where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance FromField BoundingBoxPoints where
  fromField = fromFieldBoundingBoxPoints

instance BeamSqlBackend be => B.HasSqlEqualityCheck be BoundingBoxPoints

instance FromBackendRow Postgres BoundingBoxPoints
