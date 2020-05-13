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
import Database.Beam.MySQL (MySQL, MysqlValueSyntax)
import Database.Beam.MySQL.FromField
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant
import Servant.Swagger

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

type Limit = Int

type Offset = Int

type MandatoryQueryParam name a = QueryParam' '[Required, Strict] name a
