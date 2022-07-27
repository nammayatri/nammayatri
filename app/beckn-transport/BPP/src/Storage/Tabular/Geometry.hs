{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Geometry where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import qualified Domain.Types.Geometry as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    GeometryT sql=geometry
      id Int
      region Text
      Primary id
      deriving Generic
    |]

instance TEntityKey GeometryT where
  type DomainKey GeometryT = Int
  fromKey (GeometryTKey _id) = _id
  toKey id = GeometryTKey id

instance TType GeometryT Domain.Geometry where
  fromTType GeometryT {..} = do
    return $
      Domain.Geometry
        { ..
        }
  toTType Domain.Geometry {..} =
    GeometryT
      { ..
      }
