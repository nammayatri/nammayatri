{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Storage.SearchRequest where

import Beckn.Types.Id
import Beckn.Utils.JSON
import Data.Aeson
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.Person as SP
import qualified Types.Storage.SearchReqLocation as Loc

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequestT f = SearchRequest
  { id :: B.C f (Id SearchRequest),
    startTime :: B.C f UTCTime,
    validTill :: B.C f UTCTime,
    requestorId :: B.C f (Id SP.Person),
    fromLocationId :: B.C f (Id Loc.SearchReqLocation),
    toLocationId :: B.C f (Id Loc.SearchReqLocation),
    distance :: B.C f Double,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type SearchRequest = SearchRequestT Identity

type SearchRequestPrimaryKey = B.PrimaryKey SearchRequestT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table SearchRequestT where
  data PrimaryKey SearchRequestT f = SearchRequestPrimaryKey (B.C f (Id SearchRequest))
    deriving (Generic, B.Beamable)
  primaryKey = SearchRequestPrimaryKey . id

deriving instance Show SearchRequest

deriving instance Eq SearchRequest

instance ToJSON SearchRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

instance FromJSON SearchRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity SearchRequestT)
fieldEMod =
  B.setEntityName "search_request"
    <> B.modifyTableFields
      B.tableModification
        { startTime = "start_time",
          validTill = "valid_till",
          requestorId = "requestor_id",
          fromLocationId = "from_location_id",
          toLocationId = "to_location_id",
          createdAt = "created_at"
        }
