{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Merchant where

import qualified Dashboard.Common.Merchant
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Servant
import Servant.Client

data MerchantUpdateReq = MerchantUpdateReq
  { exoPhones :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty Dashboard.Common.Merchant.ExophoneReq),
    fcmConfig :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.FCMConfigUpdateReq,
    gatewayUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    registryUrl :: Kernel.Prelude.Maybe Kernel.Prelude.BaseUrl
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> PostMerchantServiceConfigMapsUpdate))

type PostMerchantUpdate = ("update" :> ReqBody '[JSON] API.Types.RiderPlatform.Merchant.MerchantUpdateReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostMerchantServiceConfigMapsUpdate =
  ( "serviceConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: API.Types.RiderPlatform.Merchant.MerchantUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postMerchantServiceConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> postMerchantServiceConfigMapsUpdate = merchantClient
