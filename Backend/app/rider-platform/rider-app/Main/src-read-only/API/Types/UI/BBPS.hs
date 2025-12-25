{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.BBPS where

import Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BBPS
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Tools.Auth

data BBPSBillDetails = BBPSBillDetails {billerId :: Kernel.Prelude.Text, customerParams :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag], txnAmount :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSInfoAPIRes = BBPSInfoAPIRes
  { billDetails :: BBPSBillDetails,
    createdAt :: Kernel.Prelude.UTCTime,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentInformation :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag],
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentMode,
    paymentTxnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refId :: Kernel.Types.Id.Id Domain.Types.BBPS.BBPS,
    refShortId :: Kernel.Types.Id.ShortId Domain.Types.BBPS.BBPS,
    status :: Domain.Types.BBPS.BBPSPaymentStatus,
    transType :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSPaymentReq = BBPSPaymentReq
  { appId :: Kernel.Prelude.Text,
    bbpsTxnId :: Kernel.Prelude.Text,
    billDetails :: BBPSBillDetails,
    deviceId :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    transType :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSPaymentStatusAPIRes = BBPSPaymentStatusAPIRes
  { errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentInformation :: Kernel.Prelude.Maybe [Domain.Types.BBPS.Tag],
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentMode,
    paymentTxnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.BBPS.BBPSPaymentStatus
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSServerReq = BBPSServerReq
  { amount :: Kernel.Prelude.Text,
    bbps_payment_status :: Kernel.Prelude.Maybe BBPSServerStatus,
    bbps_ref_id :: Kernel.Prelude.Text,
    mobile :: Kernel.Prelude.Text,
    payment_ref_id :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSServerResp = BBPSServerResp {message :: Kernel.Prelude.Text, status :: BBPSServerStatus}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data BBPSServerStatus
  = SUCCESS
  | FAILURE
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data BBPSSessionReq = BBPSSessionReq {deviceId :: Kernel.Prelude.Text, mobileNumber :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''BBPSServerStatus)
