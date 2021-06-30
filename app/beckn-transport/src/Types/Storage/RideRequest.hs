{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.RideRequest where

import Beckn.Types.Id
import Beckn.Types.Storage.Organization
import Beckn.Utils.JSON
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend.SQL (BeamSqlBackend, FromBackendRow, HasSqlValueSyntax (..), autoSqlValueSyntax, fromBackendRow)
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude hiding (id)
import Types.App

data RideRequestType = ALLOCATION | CANCELLATION | DRIVER_RESPONSE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be RideRequestType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Postgres RideRequestType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamSqlBackend be => B.HasSqlEqualityCheck be RideRequestType

data RideRequestT f = RideRequest
  { id :: B.C f (Id RideRequest),
    rideId :: B.C f (Id Ride),
    shortOrgId :: B.C f (ShortId Organization),
    createdAt :: B.C f UTCTime,
    _type :: B.C f RideRequestType,
    info :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type RideRequest = RideRequestT Identity

type RideRequestPrimaryKey = B.PrimaryKey RideRequestT Identity

instance B.Table RideRequestT where
  data PrimaryKey RideRequestT f = RideRequestPrimaryKey (B.C f (Id Ride))
    deriving (Generic, B.Beamable)
  primaryKey = RideRequestPrimaryKey . rideId

instance ToJSON RideRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON RideRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity RideRequestT)
fieldEMod =
  B.setEntityName "ride_request"
    <> B.modifyTableFields
      B.tableModification
        { rideId = "ride_id",
          shortOrgId = "short_org_id",
          createdAt = "created_at",
          _type = "type"
        }
