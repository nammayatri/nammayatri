{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Domain.Types.PayoutRequest where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Lib.Payment.Domain.Types.Common
import Servant (FromHttpApiData (..), ToHttpApiData (..))

data PayoutRequest = PayoutRequest
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    beneficiaryId :: Kernel.Prelude.Text,
    cashMarkedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    cashMarkedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cashMarkedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    customerEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerPhone :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    customerVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Text,
    entityName :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName,
    entityRefId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expectedCreditTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutRequest.PayoutRequest,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    orderType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    retryCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    scheduledAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data PayoutRequestStatus
  = INITIATED
  | PROCESSING
  | CREDITED
  | AUTO_PAY_FAILED
  | RETRYING
  | FAILED
  | CANCELLED
  | CASH_PAID
  | CASH_PENDING
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList ''PayoutRequestStatus)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''PayoutRequestStatus)

instance FromHttpApiData [PayoutRequestStatus] where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader bs = BF.first T.pack . eitherDecode . BSL.fromStrict $ bs

instance ToHttpApiData [PayoutRequestStatus] where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
