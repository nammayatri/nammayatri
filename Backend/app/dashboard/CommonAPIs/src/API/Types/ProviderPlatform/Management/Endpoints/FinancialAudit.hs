{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.FinancialAudit where

import qualified Dashboard.Common
import Data.Aeson
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id, state)
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

-- ==========================================
-- Response types
-- ==========================================

data AuditEntryItem = AuditEntryItem
  { auditId :: Kernel.Prelude.Text,
    entityType :: Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Text,
    action :: Kernel.Prelude.Text,
    actorType :: Kernel.Prelude.Text,
    actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    previousState :: Kernel.Prelude.Maybe Data.Aeson.Value,
    newState :: Kernel.Prelude.Maybe Data.Aeson.Value,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    ipAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    hashChain :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditEntryListRes = AuditEntryListRes
  { totalItems :: Kernel.Prelude.Int,
    summary :: Dashboard.Common.Summary,
    entries :: [AuditEntryItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditDetailsRes = AuditDetailsRes
  { entry :: AuditEntryItem,
    stateDiff :: Kernel.Prelude.Maybe Data.Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditSummaryByType = AuditSummaryByType
  { entityType :: Kernel.Prelude.Text,
    count :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditSummaryByAction = AuditSummaryByAction
  { action :: Kernel.Prelude.Text,
    count :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditSummaryByActor = AuditSummaryByActor
  { actorType :: Kernel.Prelude.Text,
    count :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditSummaryRes = AuditSummaryRes
  { totalEntries :: Kernel.Prelude.Int,
    month :: Kernel.Prelude.Int,
    year :: Kernel.Prelude.Int,
    byEntityType :: [AuditSummaryByType],
    byAction :: [AuditSummaryByAction],
    byActorType :: [AuditSummaryByActor]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AuditExportReq = AuditExportReq
  { entityType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    action :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dateFrom :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    dateTo :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    format :: Kernel.Prelude.Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Dashboard.Common.HideSecrets AuditExportReq where
  hideSecrets = Kernel.Prelude.identity

data AuditExportRes = AuditExportRes
  { content :: Kernel.Prelude.Text,
    contentType :: Kernel.Prelude.Text,
    fileName :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ==========================================
-- API types
-- ==========================================

type GetFinancialAuditList =
  "finance"
    :> "audit"
    :> "list"
    :> QueryParam "entityType" Kernel.Prelude.Text
    :> QueryParam "action" Kernel.Prelude.Text
    :> QueryParam "actorType" Kernel.Prelude.Text
    :> QueryParam "actorId" Kernel.Prelude.Text
    :> QueryParam "entityId" Kernel.Prelude.Text
    :> QueryParam "dateFrom" Kernel.Prelude.UTCTime
    :> QueryParam "dateTo" Kernel.Prelude.UTCTime
    :> QueryParam "search" Kernel.Prelude.Text
    :> QueryParam "limit" Kernel.Prelude.Int
    :> QueryParam "offset" Kernel.Prelude.Int
    :> Get '[JSON] AuditEntryListRes

type GetFinancialAuditDetails =
  "finance"
    :> "audit"
    :> Capture "auditId" Kernel.Prelude.Text
    :> "details"
    :> Get '[JSON] AuditDetailsRes

type GetFinancialAuditSummary =
  "finance"
    :> "audit"
    :> "summary"
    :> QueryParam "month" Kernel.Prelude.Int
    :> QueryParam "year" Kernel.Prelude.Int
    :> Get '[JSON] AuditSummaryRes

type GetFinancialAuditAdminActions =
  "finance"
    :> "audit"
    :> "adminActions"
    :> QueryParam "dateFrom" Kernel.Prelude.UTCTime
    :> QueryParam "dateTo" Kernel.Prelude.UTCTime
    :> QueryParam "limit" Kernel.Prelude.Int
    :> QueryParam "offset" Kernel.Prelude.Int
    :> Get '[JSON] AuditEntryListRes

type PostFinancialAuditExport =
  "finance"
    :> "audit"
    :> "export"
    :> ReqBody '[JSON] AuditExportReq
    :> Post '[JSON] AuditExportRes

type FinancialAuditAPIs =
  GetFinancialAuditList
    :<|> GetFinancialAuditDetails
    :<|> GetFinancialAuditSummary
    :<|> GetFinancialAuditAdminActions
    :<|> PostFinancialAuditExport
