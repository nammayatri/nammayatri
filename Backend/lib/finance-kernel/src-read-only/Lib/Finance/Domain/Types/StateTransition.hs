{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.StateTransition where

import qualified Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id

data StateTransition = StateTransition
  { actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityType :: Kernel.Prelude.Text,
    event :: Lib.Finance.Domain.Types.StateTransition.PaymentEvent,
    eventData :: Kernel.Prelude.Maybe Data.Aeson.Value,
    fromState :: Lib.Finance.Domain.Types.StateTransition.PaymentState,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.StateTransition.StateTransition,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    toState :: Lib.Finance.Domain.Types.StateTransition.PaymentState,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data PaymentEvent = Initiate | Authorize | Capture | Settle | Fail | Refund | Cancel | PAYOUT_STATUS_CHANGED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PaymentState
  = Pending
  | Authorized
  | Captured
  | Settled
  | Failed
  | Refunded
  | Cancelled
  | INITIATED
  | PROCESSING
  | CREDITED
  | AUTO_PAY_FAILED
  | RETRYING
  | FAILED
  | CANCELLED
  | CASH_PAID
  | CASH_PENDING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentState))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentEvent))
