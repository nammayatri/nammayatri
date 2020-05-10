{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.App where

import Data.Swagger
import Database.Beam.Backend.SQL
  ( FromBackendRow,
    HasSqlValueSyntax,
  )
import Database.Beam.MySQL (MySQL, MysqlValueSyntax)
import Database.Beam.MySQL.FromField
import Epass.Utils.TH
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant
import Servant.Swagger

-- App Types
data Env = Env
  { runTime :: R.FlowRuntime
  }

type FlowHandler = ReaderT Env (ExceptT ServerError IO)

type FlowServer api = ServerT api (ReaderT Env (ExceptT ServerError IO))

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

newtype VehicleId = VehicleId
  { _getVehicleId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''VehicleId

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

newtype LocationId = LocationId
  { _getLocationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''LocationId

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a
