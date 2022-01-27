module Beckn.Storage.Esqueleto.Functions where

import Database.Esqueleto.Internal.Internal
import EulerHS.Prelude hiding (Key)

(<->.) :: SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
(<->.) = unsafeSqlBinOp " <-> "

getPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
getPoint longLat = unsafeSqlFunction "ST_SetSRID" (buildSTPoint longLat, val (4326 :: Int))

buildSTPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
buildSTPoint = unsafeSqlFunction "ST_Point"

containsPoint :: (Double, Double) -> SqlExpr (Value b)
containsPoint (lon, lat) = unsafeSqlFunction "st_contains" args
  where
    args = (unsafeSqlValue "geom", geomFromText pointText)
    geomFromText = unsafeSqlFunction "ST_GeomFromText"
    pointText = val ("POINT (" <> show lon <> " " <> show lat <> ")") :: SqlExpr (Value Text)
