{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Geometry where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id (Id (..))
import qualified Domain.Types.Geometry as Domain

mkPersist
  defaultSqlSettings
  [defaultQQ|
    GeometryT sql=geometry
      id Text
      region Text
      Primary id
      deriving Generic
    |]

instance TEntityKey GeometryT where
  type DomainKey GeometryT = Id Domain.Geometry
  fromKey (GeometryTKey _id) = Id _id
  toKey (Id id) = GeometryTKey id

instance TType GeometryT Domain.Geometry where
  fromTType GeometryT {..} = do
    return $
      Domain.Geometry
        { id = Id id,
          ..
        }
  toTType Domain.Geometry {..} =
    GeometryT
      { id = getId id,
        ..
      }
