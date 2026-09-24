{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Profile where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Profile
import qualified "this" Domain.Types.Person
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import Servant.Client

type API = ("profile" :> (GetProfileDetail :<|> PostProfileUpdate))

type GetProfileDetail = ("detail" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> Get '[JSON] Domain.Action.UI.Profile.ProfileRes)

type PostProfileUpdate =
  ( "update" :> Capture "customerId" (Kernel.Types.Id.Id Domain.Types.Person.Person) :> ReqBody '[JSON] Domain.Action.UI.Profile.UpdateProfileReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data ProfileAPIs = ProfileAPIs
  { getProfileDetail :: Kernel.Types.Id.Id Domain.Types.Person.Person -> EulerHS.Types.EulerClient Domain.Action.UI.Profile.ProfileRes,
    postProfileUpdate :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Action.UI.Profile.UpdateProfileReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkProfileAPIs :: (Client EulerHS.Types.EulerClient API -> ProfileAPIs)
mkProfileAPIs profileClient = (ProfileAPIs {..})
  where
    getProfileDetail :<|> postProfileUpdate = profileClient

data ProfileUserActionType
  = GET_PROFILE_DETAIL
  | POST_PROFILE_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''ProfileUserActionType])
