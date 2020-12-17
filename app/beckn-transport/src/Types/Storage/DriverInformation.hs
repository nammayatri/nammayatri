module Types.Storage.DriverInformation where

import Beckn.Types.Amount (Amount)
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App (DriverId)

data DriverInformationT f = DriverInformation
  { _id :: B.C f DriverId,
    _completedRidesNumber :: B.C f Int,
    _earnings :: B.C f Amount
  }
  deriving (Generic, B.Beamable)

type DriverInformation = DriverInformationT Identity

type DriverInformationPrimaryKey = B.PrimaryKey DriverInformationT Identity

instance B.Table DriverInformationT where
  data PrimaryKey DriverInformationT f = DriverInformationPrimaryKey (B.C f DriverId)
    deriving (Generic, B.Beamable)
  primaryKey = DriverInformationPrimaryKey . _id

instance ToJSON DriverInformation where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON DriverInformation where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity DriverInformationT)
fieldEMod =
  B.setEntityName "driver_information"
    <> B.modifyTableFields
      B.tableModification
        { _completedRidesNumber = "completed_rides_number"
        }
