{-
  Finance.Audit.Interface

  Input types for audit operations.
  The actual operations are in Lib.Finance.Audit.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Audit.Interface
  ( AuditInput (..),
    AuditListRequest (..),
    AuditSummaryRequest (..),
    AuditExportRequest (..),
  )
where

import Data.Aeson (Value)
import Kernel.Prelude
import Lib.Finance.Domain.Types.AuditEntry (AuditAction)

-- | Input for creating an audit entry
data AuditInput = AuditInput
  { entityType :: Text,
    entityId :: Text,
    action :: AuditAction,
    actorType :: Text,
    actorId :: Maybe Text,
    beforeState :: Maybe Value, -- Maps to previousState in DB
    afterState :: Maybe Value, -- Maps to newState in DB
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)

-- | Request for listing audit entries with filters
data AuditListRequest = AuditListRequest
  { entityType :: Maybe Text,
    action :: Maybe AuditAction,
    actorType :: Maybe Text,
    actorId :: Maybe Text,
    entityId :: Maybe Text,
    dateFrom :: Maybe UTCTime,
    dateTo :: Maybe UTCTime,
    search :: Maybe Text,
    limit :: Int,
    offset :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Request for audit summary (compliance report)
data AuditSummaryRequest = AuditSummaryRequest
  { month :: Int,
    year :: Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Request for exporting audit log
data AuditExportRequest = AuditExportRequest
  { entityType :: Maybe Text,
    action :: Maybe AuditAction,
    actorType :: Maybe Text,
    dateFrom :: Maybe UTCTime,
    dateTo :: Maybe UTCTime,
    format :: Text -- "CSV" or "JSON"
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
