{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Storage.Beam.Common where

import qualified Database.Beam as B
import GHC.Generics (Generic)
import Storage.Beam.DriverLocation
import Storage.Beam.Exophone
import Storage.Beam.Geometry
import Storage.Beam.Vehicle

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { driverLocation = dLocationTable,
        exophone = dExophone,
        geometry = geometryTable,
        vehicle = vehicleTable
      }

data AtlasDB f = AtlasDB
  { driverLocation :: f (B.TableEntity DriverLocationT),
    exophone :: f (B.TableEntity ExophoneT),
    geometry :: f (B.TableEntity GeometryT),
    vehicle :: f (B.TableEntity VehicleT)
  }
  deriving (Generic, B.Database be)
