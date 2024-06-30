{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Merchant where

import qualified Dashboard.Common.Merchant
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Servant
import Servant.Client

data MerchantUpdateReq = MerchantUpdateReq
  { description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    exoPhones :: Kernel.Prelude.Maybe (Kernel.Prelude.NonEmpty Dashboard.Common.Merchant.ExophoneReq),
    fcmConfig :: Kernel.Prelude.Maybe Dashboard.Common.Merchant.FCMConfigUpdateReq,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MerchantUpdateRes = MerchantUpdateRes
  { contactNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    enabled :: Kernel.Prelude.Bool,
    name :: Kernel.Prelude.Text,
    status :: API.Types.ProviderPlatform.Management.Merchant.Status
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Status = PENDING_VERIFICATION | APPROVED | REJECTED deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

type API = ("merchant" :> (PostMerchantUpdate :<|> PostMerchantServiceConfigMapsUpdate))

type PostMerchantUpdate =
  ( "update" :> ReqBody '[JSON] API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq
      :> Post
           '[JSON]
           API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes
  )

type PostMerchantServiceConfigMapsUpdate =
  ( "serviceConfig" :> "maps" :> "update" :> ReqBody '[JSON] Dashboard.Common.Merchant.MapsServiceConfigUpdateReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data MerchantAPIs = MerchantAPIs
  { postMerchantUpdate :: API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateReq -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.Merchant.MerchantUpdateRes,
    postMerchantServiceConfigMapsUpdate :: Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkMerchantAPIs :: (Client EulerHS.Types.EulerClient API -> MerchantAPIs)
mkMerchantAPIs merchantClient = (MerchantAPIs {..})
  where
    postMerchantUpdate :<|> postMerchantServiceConfigMapsUpdate = merchantClient
