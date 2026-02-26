{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PPFRecon where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH



data PPFRecon
    = PPFRecon {beneficiaryBankAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                beneficiaryIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                buyerAppCommission :: Kernel.Types.Common.Price,
                collectorBankAccount :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                collectorIFSC :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                collectorSubscriberId :: Kernel.Prelude.Text,
                differenceAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                domain :: Domain.Types.PPFRecon.PPFDomain,
                entityId :: Kernel.Prelude.Text,
                entityType :: Domain.Types.PPFRecon.PPFEntityType,
                fulfilledTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                id :: Kernel.Types.Id.Id Domain.Types.PPFRecon.PPFRecon,
                merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                message :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                networkFee :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                networkOrderId :: Kernel.Prelude.Text,
                orderAmount :: Kernel.Types.Common.Price,
                orderStatus :: Kernel.Prelude.Text,
                paymentAmount :: Kernel.Types.Common.Price,
                paymentReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                paymentStatus :: Domain.Types.PPFRecon.PPFPaymentStatus,
                paymentTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                receiverSubscriberId :: Kernel.Prelude.Text,
                reconCompletedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                reconInitiatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                sellerShare :: Kernel.Types.Common.Price,
                settledTimestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                settlementAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                settlementRefNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                settlementStatus :: Domain.Types.PPFRecon.PPFSettlementStatus,
                tcs :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                tds :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                transactionId :: Kernel.Prelude.Text,
                withholdingAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.Price,
                createdAt :: Kernel.Prelude.UTCTime,
                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show)
data PPFDomain = MOBILITY | FRFS deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( Kernel.Prelude.ToParamSchema))
data PPFEntityType = RIDE_BOOKING | FRFS_TICKET_BOOKING | BUS_PASS deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( Kernel.Prelude.ToParamSchema))
data PPFPaymentStatus
    = INITIATED | COLLECTED | HELD | RELEASED | SETTLED | REFUNDED | FAILED
    deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( Kernel.Prelude.ToParamSchema))
data PPFSettlementStatus = PENDING | IN_PROGRESS | SETTLEMENT_SETTLED | SETTLEMENT_FAILED deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( Kernel.Prelude.ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PPFDomain))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PPFDomain))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PPFPaymentStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PPFPaymentStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PPFSettlementStatus))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PPFSettlementStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PPFEntityType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''PPFEntityType))

