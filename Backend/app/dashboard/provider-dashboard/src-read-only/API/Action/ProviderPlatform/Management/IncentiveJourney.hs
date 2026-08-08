{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.IncentiveJourney
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.IncentiveJourney
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

type API = ("incentiveJourney" :> (GetIncentiveJourneyList :<|> PostIncentiveJourneyCreate :<|> PutIncentiveJourneyUpdate :<|> GetIncentiveJourneyMilestoneList :<|> PostIncentiveJourneyMilestoneCreate :<|> PutIncentiveJourneyMilestoneUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getIncentiveJourneyList merchantId city :<|> postIncentiveJourneyCreate merchantId city :<|> putIncentiveJourneyUpdate merchantId city :<|> getIncentiveJourneyMilestoneList merchantId city :<|> postIncentiveJourneyMilestoneCreate merchantId city :<|> putIncentiveJourneyMilestoneUpdate merchantId city

type GetIncentiveJourneyList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY) / ('API.Types.ProviderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_LIST))
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyList
  )

type PostIncentiveJourneyCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY) / ('API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_CREATE))
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyCreate
  )

type PutIncentiveJourneyUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY) / ('API.Types.ProviderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_UPDATE))
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyUpdate
  )

type GetIncentiveJourneyMilestoneList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY) / ('API.Types.ProviderPlatform.Management.IncentiveJourney.GET_INCENTIVE_JOURNEY_MILESTONE_LIST))
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.GetIncentiveJourneyMilestoneList
  )

type PostIncentiveJourneyMilestoneCreate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY) / ('API.Types.ProviderPlatform.Management.IncentiveJourney.POST_INCENTIVE_JOURNEY_MILESTONE_CREATE))
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PostIncentiveJourneyMilestoneCreate
  )

type PutIncentiveJourneyMilestoneUpdate =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.INCENTIVE_JOURNEY) / ('API.Types.ProviderPlatform.Management.IncentiveJourney.PUT_INCENTIVE_JOURNEY_MILESTONE_UPDATE))
      :> API.Types.ProviderPlatform.Management.IncentiveJourney.PutIncentiveJourneyMilestoneUpdate
  )

getIncentiveJourneyList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyListRes)
getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled driverTag = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.getIncentiveJourneyList merchantShortId opCity apiTokenInfo limit offset enabled driverTag

postIncentiveJourneyCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyRes)
postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.putIncentiveJourneyUpdate merchantShortId opCity apiTokenInfo req

getIncentiveJourneyMilestoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Types.Id.Id Dashboard.Common.IncentiveJourney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.IncentiveJourneyMilestoneListRes)
getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.getIncentiveJourneyMilestoneList merchantShortId opCity apiTokenInfo limit offset journeyId

postIncentiveJourneyMilestoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.IncentiveJourney.CreateIncentiveJourneyMilestoneRes)
postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.postIncentiveJourneyMilestoneCreate merchantShortId opCity apiTokenInfo req

putIncentiveJourneyMilestoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.IncentiveJourney.UpdateIncentiveJourneyMilestoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.IncentiveJourney.putIncentiveJourneyMilestoneUpdate merchantShortId opCity apiTokenInfo req
