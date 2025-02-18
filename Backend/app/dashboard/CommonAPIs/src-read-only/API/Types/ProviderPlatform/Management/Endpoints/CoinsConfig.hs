{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.CoinsConfig where

import qualified Dashboard.Common
import qualified Dashboard.Common.DriverCoins
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.VehicleCategory
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CreateCoinsConfigReq
  = NewCoinsConfig NewCoinsConfigReq
  | DuplicateCoinsConfig DuplicateCoinsConfigsReq
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CreateCoinsConfigReq where
  hideSecrets = Kernel.Prelude.identity

data DuplicateCoinsConfigsReq = DuplicateCoinsConfigsReq {entriesId :: Kernel.Types.Id.Id Dashboard.Common.CoinsConfig, eventFunction :: Dashboard.Common.DriverCoins.DriverCoinsFunctionType, eventMessages :: [EventMessage]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EventMessage = EventMessage {message :: Kernel.Prelude.Text, language :: Kernel.External.Types.Language}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data NewCoinsConfigReq = NewCoinsConfigReq
  { eventFunction :: Dashboard.Common.DriverCoins.DriverCoinsFunctionType,
    eventName :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOptCityId :: Kernel.Prelude.Text,
    coins :: Kernel.Prelude.Int,
    expirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    active :: Kernel.Prelude.Bool,
    vehicleCategory :: Domain.Types.VehicleCategory.VehicleCategory,
    eventMessages :: [EventMessage]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateReq = UpdateReq {entriesId :: Kernel.Types.Id.Id Dashboard.Common.CoinsConfig, active :: Kernel.Prelude.Bool, expirationAt :: Kernel.Prelude.Maybe Kernel.Prelude.Int, coins :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("coinsConfig" :> (PutCoinsConfigUpdate :<|> PostCoinsConfigCreate))

type PutCoinsConfigUpdate = ("update" :> ReqBody '[JSON] UpdateReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostCoinsConfigCreate = ("create" :> ReqBody '[JSON] CreateCoinsConfigReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data CoinsConfigAPIs = CoinsConfigAPIs
  { putCoinsConfigUpdate :: UpdateReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postCoinsConfigCreate :: CreateCoinsConfigReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkCoinsConfigAPIs :: (Client EulerHS.Types.EulerClient API -> CoinsConfigAPIs)
mkCoinsConfigAPIs coinsConfigClient = (CoinsConfigAPIs {..})
  where
    putCoinsConfigUpdate :<|> postCoinsConfigCreate = coinsConfigClient

data CoinsConfigUserActionType
  = PUT_COINS_CONFIG_UPDATE
  | POST_COINS_CONFIG_CREATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''CoinsConfigUserActionType])
