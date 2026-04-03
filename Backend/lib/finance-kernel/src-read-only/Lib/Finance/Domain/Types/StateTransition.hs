{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Finance.Domain.Types.StateTransition where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH



data StateTransition
    = StateTransition {actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       actorType :: Kernel.Prelude.Text,
                       createdAt :: Kernel.Prelude.UTCTime,
                       entityId :: Kernel.Prelude.Text,
                       entityType :: Lib.Finance.Domain.Types.StateTransition.PaymentEntityType,
                       event :: Lib.Finance.Domain.Types.StateTransition.PaymentEvent,
                       eventData :: Kernel.Prelude.Maybe Data.Aeson.Value,
                       fromState :: Lib.Finance.Domain.Types.StateTransition.PaymentState,
                       id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.StateTransition.StateTransition,
                       merchantId :: Kernel.Prelude.Text,
                       merchantOperatingCityId :: Kernel.Prelude.Text,
                       toState :: Lib.Finance.Domain.Types.StateTransition.PaymentState,
                       updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
data PaymentEntityType = PaymentTransaction | PayoutRequest | Refunds deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
data PaymentEvent = INITIATE | AUTHORIZE | CAPTURE | SETTLE | FAIL | REFUND | CANCEL | CREDIT | RETRY deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
data PaymentState
    = PENDING | AUTHORIZED | CAPTURED | SETTLED | FAILED | REFUNDED | CANCELLED | INITIATED | PROCESSING | CREDITED | AUTO_PAY_FAILED | RETRYING | CASH_PAID | CASH_PENDING
    deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentEntityType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentEvent))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentState))

