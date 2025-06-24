{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.NyRegularSubscription
  ( API,
    handler,
  )
where

import qualified API.Types.UI.NyRegularSubscription
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.NyRegularSubscription as Domain.Action.UI.NyRegularSubscription
import qualified Domain.Action.UI.Quote
import qualified Domain.Types.Client
import qualified Domain.Types.Merchant
import qualified Domain.Types.NyRegularSubscription
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "nyRegular" :> "subscriptions" :> "create" :> Header "client-id" (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client))
      :> Header
           "is-dashboard-request"
           Kernel.Prelude.Bool
      :> Header "x-bundle-version" (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
      :> Header
           "x-client-version"
           (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
      :> Header
           "x-config-version"
           (Kernel.Prelude.Maybe Kernel.Types.Version.Version)
      :> Header
           "x-device"
           (Kernel.Prelude.Maybe Data.Text.Text)
      :> Header
           "x-rn-version"
           (Kernel.Prelude.Maybe Data.Text.Text)
      :> ReqBody
           '[JSON]
           API.Types.UI.NyRegularSubscription.CreateSubscriptionReq
      :> Post
           '[JSON]
           API.Types.UI.NyRegularSubscription.CreateSubscriptionRes
      :<|> TokenAuth
      :> "nyRegular"
      :> "subscriptions"
      :> "estimate"
      :> Capture
           "searchRequestId"
           Data.Text.Text
      :> Get
           '[JSON]
           Domain.Action.UI.Quote.GetQuotesRes
      :<|> TokenAuth
      :> "nyRegular"
      :> "subscriptions"
      :> "confirm"
      :> ReqBody
           '[JSON]
           API.Types.UI.NyRegularSubscription.ConfirmSubscriptionReq
      :> Post
           '[JSON]
           Domain.Types.NyRegularSubscription.NyRegularSubscription
      :<|> TokenAuth
      :> "nyRegular"
      :> "subscriptions"
      :> "update"
      :> ReqBody
           '[JSON]
           API.Types.UI.NyRegularSubscription.UpdateSubscriptionReq
      :> Post
           '[JSON]
           Domain.Types.NyRegularSubscription.NyRegularSubscription
      :<|> TokenAuth
      :> "nyRegular"
      :> "subscriptions"
      :> QueryParam
           "status"
           (Kernel.Prelude.Maybe Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus)
      :> QueryParam
           "limit"
           (Kernel.Prelude.Maybe Kernel.Prelude.Int)
      :> QueryParam
           "offset"
           (Kernel.Prelude.Maybe Kernel.Prelude.Int)
      :> Get
           '[JSON]
           [Domain.Types.NyRegularSubscription.NyRegularSubscription]
      :<|> TokenAuth
      :> "nyRegular"
      :> "subscriptionDetails"
      :> Capture
           "subscriptionId"
           (Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription)
      :> Get
           '[JSON]
           Domain.Types.NyRegularSubscription.NyRegularSubscription
      :<|> TokenAuth
      :> "nyRegular"
      :> "subscriptions"
      :> Capture
           "subscriptionId"
           (Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription)
      :> "cancel"
      :> Post
           '[JSON]
           Domain.Types.NyRegularSubscription.NyRegularSubscription
  )

handler :: Environment.FlowServer API
handler = postNyRegularSubscriptionsCreate :<|> getNyRegularSubscriptionsEstimate :<|> postNyRegularSubscriptionsConfirm :<|> postNyRegularSubscriptionsUpdate :<|> getNyRegularSubscriptions :<|> getNyRegularSubscriptionDetails :<|> postNyRegularSubscriptionsCancel

postNyRegularSubscriptionsCreate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Client.Client)) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Types.Version.Version) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Data.Text.Text) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Data.Text.Text) ->
    API.Types.UI.NyRegularSubscription.CreateSubscriptionReq ->
    Environment.FlowHandler API.Types.UI.NyRegularSubscription.CreateSubscriptionRes
  )
postNyRegularSubscriptionsCreate a9 a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.postNyRegularSubscriptionsCreate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a9) a8 a7 a6 a5 a4 a3 a2 a1

getNyRegularSubscriptionsEstimate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler Domain.Action.UI.Quote.GetQuotesRes
  )
getNyRegularSubscriptionsEstimate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.getNyRegularSubscriptionsEstimate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postNyRegularSubscriptionsConfirm ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.ConfirmSubscriptionReq ->
    Environment.FlowHandler Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsConfirm a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.postNyRegularSubscriptionsConfirm (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postNyRegularSubscriptionsUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NyRegularSubscription.UpdateSubscriptionReq ->
    Environment.FlowHandler Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsUpdate a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.postNyRegularSubscriptionsUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getNyRegularSubscriptions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Domain.Types.NyRegularSubscription.NyRegularSubscriptionStatus) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Maybe Kernel.Prelude.Int) ->
    Environment.FlowHandler [Domain.Types.NyRegularSubscription.NyRegularSubscription]
  )
getNyRegularSubscriptions a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.getNyRegularSubscriptions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1

getNyRegularSubscriptionDetails ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription ->
    Environment.FlowHandler Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
getNyRegularSubscriptionDetails a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.getNyRegularSubscriptionDetails (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postNyRegularSubscriptionsCancel ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.NyRegularSubscription.NyRegularSubscription ->
    Environment.FlowHandler Domain.Types.NyRegularSubscription.NyRegularSubscription
  )
postNyRegularSubscriptionsCancel a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.NyRegularSubscription.postNyRegularSubscriptionsCancel (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
