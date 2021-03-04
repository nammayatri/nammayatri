{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.ExternalTrail where

import Data.Aeson
import qualified Database.Beam as B
import EulerHS.Prelude

data ExternalTrailT f = ExternalTrail
  { _id :: B.C f Text,
    _gatewayId :: B.C f Text,
    --, _customerId  :: B.C f (Maybe (ID Customer))
    --, _sessionId   :: B.C f (Maybe SessionId)
    _endpointId :: B.C f LText,
    _headers :: B.C f LText,
    _queryParams :: B.C f LText,
    _request :: B.C f (Maybe LText),
    _succeeded :: B.C f (Maybe Bool),
    _response :: B.C f (Maybe Text),
    _error :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

type ExternalTrail = ExternalTrailT Identity

type ExternalTrailPrimaryKey = B.PrimaryKey ExternalTrailT Identity

instance B.Table ExternalTrailT where
  data PrimaryKey ExternalTrailT f
    = ExternalTrailPrimaryKey (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = ExternalTrailPrimaryKey . _id

deriving instance Show ExternalTrail

deriving instance Eq ExternalTrail

deriving instance ToJSON ExternalTrail

deriving instance FromJSON ExternalTrail

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity ExternalTrailT)
fieldEMod =
  B.setEntityName "external_trail"
    <> B.modifyTableFields
      B.tableModification
        { _gatewayId = "gateway_id",
          --, _customerId = "customer_id"
          --, _sessionId = "session_id"
          _endpointId = "endpoint_id",
          _queryParams = "query_params"
        }
