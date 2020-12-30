module Types.Storage.DriverInformation where

import Beckn.Types.Amount (Amount)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App (DriverId)

data DriverInformationT f = DriverInformation
  { _driverId :: B.C f DriverId,
    _completedRidesNumber :: B.C f Int,
    _earnings :: B.C f Amount,
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type DriverInformation = DriverInformationT Identity

type DriverInformationPrimaryKey = B.PrimaryKey DriverInformationT Identity

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f = DriverInformationPrimaryKey (B.C f DriverId)
    deriving (Generic, B.Beamable)
  primaryKey = DriverInformationPrimaryKey . _driverId

instance ToJSON DriverInformation where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON DriverInformation where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverInformationT)
fieldEMod =
  B.setEntityName "driver_information_cache"
    <> B.modifyTableFields
      B.tableModification
        { _driverId = "driver_id",
          _completedRidesNumber = "completed_rides_number",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
