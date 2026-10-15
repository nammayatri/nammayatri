{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SupportBot
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SupportBot
import qualified Control.Lens
import qualified Data.Aeson
import qualified Data.Text
import qualified Domain.Action.UI.SupportBot
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
import qualified Tools.ActorInfo
import Tools.Auth

type API =
  ( TokenAuth :> "support" :> "bot" :> "conversation" :> ReqBody ('[JSON]) API.Types.UI.SupportBot.SupportBotConversationReq
      :> Post
           ('[JSON])
           Data.Aeson.Value
      :<|> TokenAuth
      :> "support"
      :> "bot"
      :> "conversation"
      :> Capture "conversationId" Data.Text.Text
      :> "end"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postSupportBotConversation :<|> postSupportBotConversationEnd

postSupportBotConversation ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.SupportBot.SupportBotConversationReq ->
    Environment.FlowHandler Data.Aeson.Value
  )
postSupportBotConversation a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SupportBot.postSupportBotConversation (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postSupportBotConversationEnd ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Data.Text.Text ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postSupportBotConversationEnd a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.SupportBot.postSupportBotConversationEnd (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
