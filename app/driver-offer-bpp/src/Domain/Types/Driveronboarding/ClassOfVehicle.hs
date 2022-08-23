module Domain.Types.DriverOnboarding.ClassOfVehicle where

import Beckn.Prelude
import Data.Aeson

data VerificationStatus = PENDING | VALID | INVALID
  deriving (Show, Eq, Read, Generic, Enum, Bounded, FromJSON, ToJSON, ToSchema)

data ClassOfVehicle = W_NT | W_T | W_CAB | HGV_T | HMV_HGV | HMV | HTV | LMV | LMV_NT | LMV_T | LMV_CAB | LMV_HMV | LTV | MCWG | MCWOG | HPMV | MGV | MMV | LDRXCV | PSV_BUS | TRANS | TRCTOR | Others
  deriving (Show, Eq, Read, Generic, Enum, Bounded, Ord, ToSchema)

instance FromJSON ClassOfVehicle where
  parseJSON = genericParseJSON constructorForCOVToJson

instance ToJSON ClassOfVehicle where
  toJSON = genericToJSON constructorForCOVToJson

constructorForCOVToJson :: Options
constructorForCOVToJson =
  defaultOptions
    { constructorTagModifier = \case
        "W_NT" -> "3W_NT"
        "W_T" -> "3W_T"
        "W_CAB" -> "3W_CAB"
        val -> val
    }
