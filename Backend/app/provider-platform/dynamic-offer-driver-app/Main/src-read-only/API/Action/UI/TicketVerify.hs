{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.TicketVerify
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Data.Aeson
import qualified Data.Text
import qualified Domain.Action.UI.TicketVerify
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "multimodal" :> "ticket" :> "verify" :> MandatoryQueryParam "city" Data.Text.Text :> ReqBody ('[JSON]) Data.Aeson.Value :> Post ('[JSON]) Data.Aeson.Value)

handler :: Environment.FlowServer API
handler = postMultimodalTicketVerify

postMultimodalTicketVerify ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Data.Aeson.Value ->
    Environment.FlowHandler Data.Aeson.Value
  )
postMultimodalTicketVerify a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.TicketVerify.postMultimodalTicketVerify (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
