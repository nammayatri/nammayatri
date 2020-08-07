{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Storage.Trail where

import Data.Aeson
import Data.Time
import Data.Time.Units (Millisecond)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import EulerHS.Prelude

data TrailT f = Trail
  { _id :: B.C f Text,
    -- _customerId      :: B.C f (Maybe CustomerId),
    -- _sessionId       :: B.C f (Maybe SessionId),
    _endpointId :: B.C f LText,
    _headers :: B.C f LText,
    _queryParams :: B.C f LText,
    _requestBody :: B.C f (Maybe Text),
    _remoteHost :: B.C f LText,
    _isSecure :: B.C f Bool,
    _succeeded :: B.C f (Maybe Bool),
    _responseStatus :: B.C f (Maybe LText),
    _responseBody :: B.C f (Maybe Text),
    _responseHeaders :: B.C f (Maybe LText),
    _createdAt :: B.C f LocalTime,
    _processDuration :: B.C f (Maybe Millisecond)
    -- TODO: is it convenient to store durration in mcs?
    -- maybe use NominalDiffTime (fractional seconds) instead?
  }
  deriving (Generic, B.Beamable)

-- TODO: some of the fields above should be 'LByteString's, but euler-hs makes
-- it hard to use bytestrings by requiring 'JSONEx' constraint from fields
-- (ByteString has no JSON instances)
-- Since we don't need to store the data precisely, let's convert bytestring
-- to 'Text'.

type Trail = TrailT Identity

type TrailPrimaryKey = B.PrimaryKey TrailT Identity

instance B.Table TrailT where
  data PrimaryKey TrailT f = TrailPrimaryKey (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = TrailPrimaryKey . _id

deriving instance Show Trail

deriving instance Eq Trail

deriving instance ToJSON Trail

deriving instance FromJSON Trail

instance ToJSON Millisecond where
  toJSON = toJSON . toInteger

instance FromJSON Millisecond where
  parseJSON = fmap fromInteger . parseJSON

instance HasSqlValueSyntax PgValueSyntax Millisecond where
  sqlValueSyntax = sqlValueSyntax . fromIntegral @_ @Int64

instance FromBackendRow Postgres Millisecond where
  fromBackendRow = fromIntegral @Int64 <$> fromBackendRow

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TrailT)
fieldEMod =
  B.setEntityName "trail"
    <> B.modifyTableFields
      B.tableModification
        { -- _customerId = "customer_id",
          -- _sessionId = "session_id",
          _endpointId = "endpoint_id",
          _queryParams = "query_params",
          _requestBody = "request_body",
          _remoteHost = "remote_host",
          _isSecure = "is_secure",
          _responseBody = "response_body",
          _responseStatus = "response_status",
          _responseHeaders = "response_headers",
          _createdAt = "created_at",
          _processDuration = "process_duration"
        }
