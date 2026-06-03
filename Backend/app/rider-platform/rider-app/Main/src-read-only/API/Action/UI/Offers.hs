{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Offers
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.Offers
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Offer
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "offers" :> "list" :> QueryParam "amount" Kernel.Types.Common.HighPrecMoney :> Get ('[JSON]) [SharedLogic.Offer.OfferRespAPIEntity])

handler :: Environment.FlowServer API
handler = getOffersList

getOffersList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) ->
    Environment.FlowHandler [SharedLogic.Offer.OfferRespAPIEntity]
  )
getOffersList a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Offers.getOffersList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
