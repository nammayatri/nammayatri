{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.API.Payout.Types where

import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Common as Common
import Kernel.Types.HideSecrets
import qualified Kernel.Types.Id as Id
import Lib.Payment.Domain.Types.Common (EntityName)
import Lib.Payment.Domain.Types.PayoutRequest (PayoutRequest, PayoutRequestStatus)

data PayoutStatusEvent = PayoutStatusEvent
  { status :: PayoutRequestStatus,
    timestamp :: UTCTime,
    message :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutRequestResp = PayoutRequestResp
  { payoutRequestId :: Id.Id PayoutRequest,
    entityName :: Maybe EntityName,
    entityId :: Text,
    entityRefId :: Maybe Text,
    beneficiaryId :: Text,
    amount :: Maybe Common.HighPrecMoney,
    status :: PayoutRequestStatus,
    retryCount :: Maybe Int,
    failureReason :: Maybe Text,
    payoutTransactionId :: Maybe Text,
    expectedCreditTime :: Maybe UTCTime,
    scheduledAt :: Maybe UTCTime,
    cashMarkedById :: Maybe Text,
    cashMarkedByName :: Maybe Text,
    cashMarkedAt :: Maybe UTCTime,
    statusHistory :: [PayoutStatusEvent],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutCashUpdateReq = PayoutCashUpdateReq
  { status :: PayoutRequestStatus,
    agentId :: Text,
    agentName :: Text,
    message :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets PayoutCashUpdateReq where
  hideSecrets = Kernel.Prelude.identity

data PayoutCancelReq = PayoutCancelReq
  { reason :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets PayoutCancelReq where
  hideSecrets = Kernel.Prelude.identity

data DeleteVpaReq = DeleteVpaReq
  { personIds :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets DeleteVpaReq where
  hideSecrets = Kernel.Prelude.identity

data UpdateVpaReq = UpdateVpaReq
  { personId :: Text,
    vpa :: Text,
    verify :: Bool,
    vpaStatus :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UpdateVpaReq where
  hideSecrets = Kernel.Prelude.identity

data RefundRegAmountReq = RefundRegAmountReq
  { personId :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets RefundRegAmountReq where
  hideSecrets = Kernel.Prelude.identity

type PayoutSuccess = APISuccess.APISuccess
