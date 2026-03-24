{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.RefundRequest (module Domain.Types.RefundRequest, module ReExport) where
import Kernel.Prelude
import Kernel.Utils.TH
import Data.Aeson
import Domain.Types.Extra.RefundRequest as ReExport
import qualified Domain.Types.Extra.RefundRequest
import qualified Kernel.Utils.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Lib.Payment.Domain.Types.PaymentOrder
import qualified Domain.Types.Person
import qualified Kernel.Types.Common
import qualified Lib.Payment.Domain.Types.Refunds
import qualified Lib.Payment.Domain.Types.PaymentTransaction
import qualified Tools.Beam.UtilsTH



data RefundRequest
    = RefundRequest {code :: Domain.Types.Extra.RefundRequest.RefundRequestCode,
                     currency :: Kernel.Utils.Common.Currency,
                     description :: Kernel.Prelude.Text,
                     evidenceS3Path :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                     id :: Kernel.Types.Id.Id Domain.Types.RefundRequest.RefundRequest,
                     merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                     orderId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentOrder.PaymentOrder,
                     personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                     refundPurpose :: Domain.Types.RefundRequest.RefundPurpose,
                     refundsAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                     refundsId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Payment.Domain.Types.Refunds.Refunds),
                     refundsTries :: Kernel.Prelude.Int,
                     requestedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                     responseDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                     status :: Domain.Types.RefundRequest.RefundRequestStatus,
                     transactionAmount :: Kernel.Types.Common.HighPrecMoney,
                     transactionId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PaymentTransaction.PaymentTransaction,
                     createdAt :: Kernel.Prelude.UTCTime,
                     updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data RefundPurpose = RIDE_FARE | CANCELLATION_FEE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)
data RefundRequestStatus = OPEN | APPROVED | REJECTED | FAILED | REFUNDED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RefundPurpose))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''RefundRequestStatus))

$(mkHttpInstancesForEnum (''RefundRequestStatus))

