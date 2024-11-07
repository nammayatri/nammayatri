{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.HotSpot where

import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

type API = PostHotSpotRemoveExpired

type PostHotSpotRemoveExpired = ("removeExpired" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

newtype HotSpotAPIs = HotSpotAPIs {postHotSpotRemoveExpired :: EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess}

mkHotSpotAPIs :: (Client EulerHS.Types.EulerClient API -> HotSpotAPIs)
mkHotSpotAPIs hotSpotClient = (HotSpotAPIs {..})
  where
    postHotSpotRemoveExpired = hotSpotClient

data HotSpotUserActionType
  = POST_HOT_SPOT_REMOVE_EXPIRED
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON HotSpotUserActionType where
  toJSON POST_HOT_SPOT_REMOVE_EXPIRED = Data.Aeson.String "POST_HOT_SPOT_REMOVE_EXPIRED"

instance FromJSON HotSpotUserActionType where
  parseJSON (Data.Aeson.String "POST_HOT_SPOT_REMOVE_EXPIRED") = pure POST_HOT_SPOT_REMOVE_EXPIRED
  parseJSON _ = fail "POST_HOT_SPOT_REMOVE_EXPIRED expected"

$(Data.Singletons.TH.genSingletons [''HotSpotUserActionType])
