module Types.Storage.DriverInformation where

import Beckn.Types.Id
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.Storage.Person (Person)

data DriverInformationT f = DriverInformation
  { driverId :: B.C f (Id Person),
    active :: B.C f Bool,
    onRide :: B.C f Bool,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverInformation = DriverInformationT Identity

type DriverInformationPrimaryKey = B.PrimaryKey DriverInformationT Identity

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f = DriverInformationPrimaryKey (B.C f (Id Person))
    deriving (Generic, B.Beamable)
  primaryKey = DriverInformationPrimaryKey . driverId

deriving instance FromJSON DriverInformation

deriving instance ToJSON DriverInformation

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverInformationT)
fieldEMod =
  B.setEntityName "driver_information"
    <> B.modifyTableFields
      B.tableModification
        { driverId = "driver_id",
          onRide = "on_ride",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }
