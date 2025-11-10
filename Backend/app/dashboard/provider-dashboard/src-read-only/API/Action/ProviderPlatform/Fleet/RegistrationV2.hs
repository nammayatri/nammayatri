{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet.RegistrationV2
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2
import qualified Domain.Action.ProviderPlatform.Fleet.RegistrationV2
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("fleet" :> (PostRegistrationV2LoginOtp :<|> PostRegistrationV2VerifyOtp :<|> PostRegistrationV2Register :<|> GetRegistrationV2FleetMembers :<|> PostRegistrationV2FleetMembersUnlink :<|> DeleteRegistrationV2FleetMember))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRegistrationV2LoginOtp merchantId city :<|> postRegistrationV2VerifyOtp merchantId city :<|> postRegistrationV2Register merchantId city :<|> getRegistrationV2FleetMembers merchantId city :<|> postRegistrationV2FleetMembersUnlink merchantId city :<|> deleteRegistrationV2FleetMember merchantId city

type PostRegistrationV2LoginOtp = API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2LoginOtp

type PostRegistrationV2VerifyOtp = API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2VerifyOtp

type PostRegistrationV2Register =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.REGISTRATION_V2 / 'API.Types.ProviderPlatform.Fleet.RegistrationV2.POST_REGISTRATION_V2_REGISTER)
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2Register
  )

type GetRegistrationV2FleetMembers =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.REGISTRATION_V2 / 'API.Types.ProviderPlatform.Fleet.RegistrationV2.GET_REGISTRATION_V2_FLEET_MEMBERS)
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.GetRegistrationV2FleetMembers
  )

type PostRegistrationV2FleetMembersUnlink =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.REGISTRATION_V2 / 'API.Types.ProviderPlatform.Fleet.RegistrationV2.POST_REGISTRATION_V2_FLEET_MEMBERS_UNLINK)
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2FleetMembersUnlink
  )

type DeleteRegistrationV2FleetMember =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.REGISTRATION_V2 / 'API.Types.ProviderPlatform.Fleet.RegistrationV2.DELETE_REGISTRATION_V2_FLEET_MEMBER)
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.DeleteRegistrationV2FleetMember
  )

postRegistrationV2LoginOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2LoginOtp merchantShortId opCity fleetOwnerId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2LoginOtp merchantShortId opCity fleetOwnerId req

postRegistrationV2VerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyReqV2 -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyResV2)
postRegistrationV2VerifyOtp merchantShortId opCity fleetOwnerId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2VerifyOtp merchantShortId opCity fleetOwnerId req

postRegistrationV2Register :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2Register merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2Register merchantShortId opCity apiTokenInfo req

getRegistrationV2FleetMembers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetMemberAssociationRes)
getRegistrationV2FleetMembers merchantShortId opCity apiTokenInfo limit offset from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.getRegistrationV2FleetMembers merchantShortId opCity apiTokenInfo limit offset from to

postRegistrationV2FleetMembersUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2FleetMembersUnlink merchantShortId opCity apiTokenInfo fleetMemberId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.postRegistrationV2FleetMembersUnlink merchantShortId opCity apiTokenInfo fleetMemberId

deleteRegistrationV2FleetMember :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteRegistrationV2FleetMember merchantShortId opCity apiTokenInfo fleetMemberId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.RegistrationV2.deleteRegistrationV2FleetMember merchantShortId opCity apiTokenInfo fleetMemberId
