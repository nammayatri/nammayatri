{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.App where

import Beckn.Utils.TH
import Data.Swagger
import Database.Beam.Backend.SQL
  ( FromBackendRow,
    HasSqlValueSyntax,
  )
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import EulerHS.Prelude

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
