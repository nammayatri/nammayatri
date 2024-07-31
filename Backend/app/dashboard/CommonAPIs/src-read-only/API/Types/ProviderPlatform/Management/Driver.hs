{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Driver where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Servant.Client

data AadhaarDetails = AadhaarDetails {aadhaarStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, aadhaarStatusTime :: Kernel.Prelude.UTCTime, aadhaarTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AadharPanSyncReq = AadharPanSyncReq {countryCode :: Kernel.Prelude.Text, documentType :: API.Types.ProviderPlatform.Management.Driver.SyncDocType, phoneNo :: Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PanAadharSelfieDetailsResp = PanAadharSelfieDetailsResp
  { aadhaarDetails :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.AadhaarDetails,
    panDetails :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.PanDetails,
    personId :: Kernel.Prelude.Text,
    personName :: Kernel.Prelude.Text,
    selfieDetails :: Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.Driver.SelfieDetails
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PanDetails = PanDetails {panStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, panStatusTime :: Kernel.Prelude.UTCTime, panTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SelfieDetails = SelfieDetails {latestStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text, latestStatusTime :: Kernel.Prelude.UTCTime, latestTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SyncDocType = Aadhaar | Pan deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

type API = ("driver" :> (GetDriverPanAadharSelfieDetails :<|> PostDriverSyncDocAadharPan))

type GetDriverPanAadharSelfieDetails =
  ( "panAadharSelfieDetails" :> MandatoryQueryParam "countryCode" Kernel.Prelude.Text :> MandatoryQueryParam "phoneNo" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp
  )

type PostDriverSyncDocAadharPan = ("syncDocAadharPan" :> ReqBody ('[JSON]) API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data DriverAPIs = DriverAPIs
  { getDriverPanAadharSelfieDetails :: (Kernel.Prelude.Text -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Driver.PanAadharSelfieDetailsResp),
    postDriverSyncDocAadharPan :: (API.Types.ProviderPlatform.Management.Driver.AadharPanSyncReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkDriverAPIs :: (Client EulerHS.Types.EulerClient API -> DriverAPIs)
mkDriverAPIs driverClient = (DriverAPIs {..})
  where
    getDriverPanAadharSelfieDetails :<|> postDriverSyncDocAadharPan = driverClient
