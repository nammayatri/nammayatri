module Beckn.Storage.Esqueleto.Functions where

import Database.Esqueleto.Internal.Internal
import EulerHS.Prelude hiding (Key)

(<->.) :: SqlExpr (Value a) -> SqlExpr (Value b) -> SqlExpr (Value c)
(<->.) = unsafeSqlBinOp " <-> "

getPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
getPoint longLat = unsafeSqlFunction "ST_SetSRID" (buildSTPoint longLat, val (4326 :: Int))

buildSTPoint :: (SqlExpr (Value Double), SqlExpr (Value Double)) -> SqlExpr (Value b)
buildSTPoint = unsafeSqlFunction "ST_Point"
