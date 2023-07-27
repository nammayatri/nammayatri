{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Storage.Beam.Common where

import qualified Database.Beam as B
import GHC.Generics (Generic)
import Storage.Beam.Booking
import Storage.Beam.Exophone
import Storage.Beam.Geometry as BeamG
import Storage.Beam.Person
import Storage.Beam.Ride

atlasDB :: B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { exophone = dExophone,
        geometry = geometryTable,
        booking = bookingTable,
        ride = rideTable,
        person = personTable
      }

data AtlasDB f = AtlasDB
  { exophone :: f (B.TableEntity ExophoneT),
    geometry :: f (B.TableEntity GeometryT),
    booking :: f (B.TableEntity BookingT),
    ride :: f (B.TableEntity RideT),
    person :: f (B.TableEntity PersonT)
  }
  deriving (Generic, B.Database be)
