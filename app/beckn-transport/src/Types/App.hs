{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.App where

import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.TH
import EulerHS.Prelude
import Servant

newtype CustomerId = CustomerId
  { _getCustomerId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''CustomerId

newtype LeadsId = LeadsId
  { _getLeadsId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''LeadsId

newtype QuotationId = QuotationId
  { _getQuotationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''QuotationId

newtype TripReferenceId = TripReferenceId
  { _getTripReferenceId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''TripReferenceId

newtype DriverId = DriverId
  { _getDriverId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''DriverId

newtype TrackerId = TrackerId
  { _getTrackerId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''TrackerId

newtype TranposrterParameterId = TranposrterParameterId
  { _getTranposrterParameterId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''TranposrterParameterId

newtype ConfigKey = ConfigKey
  { _getConfigKey :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ConfigKey

newtype RideId = RideId
  { _getRideId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''RideId

newtype RideRequestId = RideRequestId
  { _getRideRequestId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''RideRequestId

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a

data SortMode
  = ETA
  | IdleTime
  deriving (Eq, Generic, FromDhall)
