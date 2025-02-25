{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MultimodalConfirm
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MultimodalConfirm
import qualified Control.Lens
import qualified Domain.Action.UI.MultimodalConfirm as Domain.Action.UI.MultimodalConfirm
import qualified Domain.Types.Journey
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "multimodal" :> Capture "journeyId" (Kernel.Types.Id.Id Domain.Types.Journey.Journey) :> "initiate"
      :> Post
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyInfoResp
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "confirm"
      :> QueryParam
           "forceBookLegOrder"
           Kernel.Prelude.Int
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyConfirmReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "booking"
      :> "info"
      :> Get
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyInfoResp
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "booking"
      :> "paymentStatus"
      :> Get
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyBookingPaymentStatus
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "switch"
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.SwitchLegReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> "journey"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "leg"
      :> Capture
           "legOrder"
           Kernel.Prelude.Int
      :> "skip"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> "journey"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "leg"
      :> Capture
           "legOrder"
           Kernel.Prelude.Int
      :> "addSkippedLeg"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> "extend"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "leg"
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.ExtendLegReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> "extend"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "leg"
      :> "getfare"
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.ExtendLegGetFareReq
      :> Post
           '[JSON]
           API.Types.UI.MultimodalConfirm.ExtendLegGetFareResp
      :<|> TokenAuth
      :> "multimodal"
      :> "journey"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "status"
      :> Get
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyStatusResp
      :<|> TokenAuth
      :> "multimodal"
      :> "journey"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "cancel"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "rider"
      :> "location"
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.RiderLocationReq
      :> Post
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyStatusResp
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "order"
      :> Capture
           "legOrder"
           Kernel.Prelude.Int
      :> "switchTaxi"
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.SwitchTaxiReq
      :> Post
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyInfoResp
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "journeyFeedback"
      :> ReqBody
           '[JSON]
           API.Types.UI.MultimodalConfirm.JourneyFeedBackForm
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "multimodal"
      :> Capture
           "journeyId"
           (Kernel.Types.Id.Id Domain.Types.Journey.Journey)
      :> "feedback"
      :> Get
           '[JSON]
           (Kernel.Prelude.Maybe API.Types.UI.MultimodalConfirm.JourneyFeedBackForm)
  )

handler :: Environment.FlowServer API
handler = postMultimodalInitiate :<|> postMultimodalConfirm :<|> getMultimodalBookingInfo :<|> getMultimodalBookingPaymentStatus :<|> postMultimodalSwitch :<|> postMultimodalJourneyLegSkip :<|> postMultimodalJourneyLegAddSkippedLeg :<|> postMultimodalExtendLeg :<|> postMultimodalExtendLegGetfare :<|> getMultimodalJourneyStatus :<|> postMultimodalJourneyCancel :<|> postMultimodalRiderLocation :<|> postMultimodalOrderSwitchTaxi :<|> postMultimodalJourneyFeedback :<|> getMultimodalFeedback

postMultimodalInitiate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalInitiate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalInitiate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postMultimodalConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.JourneyConfirmReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalConfirm a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getMultimodalBookingInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
getMultimodalBookingInfo a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.getMultimodalBookingInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getMultimodalBookingPaymentStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyBookingPaymentStatus
  )
getMultimodalBookingPaymentStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.getMultimodalBookingPaymentStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postMultimodalSwitch ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.SwitchLegReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalSwitch a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalSwitch (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalJourneyLegSkip ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegSkip a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalJourneyLegSkip (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalJourneyLegAddSkippedLeg ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyLegAddSkippedLeg a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalJourneyLegAddSkippedLeg (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalExtendLeg ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.ExtendLegReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalExtendLeg a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalExtendLeg (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalExtendLegGetfare ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.ExtendLegGetFareReq ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.ExtendLegGetFareResp
  )
postMultimodalExtendLegGetfare a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalExtendLegGetfare (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getMultimodalJourneyStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyStatusResp
  )
getMultimodalJourneyStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.getMultimodalJourneyStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postMultimodalJourneyCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalJourneyCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postMultimodalRiderLocation ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.RiderLocationReq ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyStatusResp
  )
postMultimodalRiderLocation a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalRiderLocation (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postMultimodalOrderSwitchTaxi ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Kernel.Prelude.Int ->
    API.Types.UI.MultimodalConfirm.SwitchTaxiReq ->
    Environment.FlowHandler API.Types.UI.MultimodalConfirm.JourneyInfoResp
  )
postMultimodalOrderSwitchTaxi a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalOrderSwitchTaxi (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

postMultimodalJourneyFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    API.Types.UI.MultimodalConfirm.JourneyFeedBackForm ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postMultimodalJourneyFeedback a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.postMultimodalJourneyFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getMultimodalFeedback ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Journey.Journey ->
    Environment.FlowHandler (Kernel.Prelude.Maybe API.Types.UI.MultimodalConfirm.JourneyFeedBackForm)
  )
getMultimodalFeedback a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MultimodalConfirm.getMultimodalFeedback (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
