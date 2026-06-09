module API.UI.Rewards where

import qualified Domain.Action.UI.Rewards as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "rewards"
    :> "offers"
    :> TokenAuth
    :> Get '[JSON] [Domain.RewardOfferResp]

handler :: FlowServer API
handler = getRewardOffersHandler

getRewardOffersHandler :: (Id SP.Person, Id DM.Merchant) -> FlowHandler [Domain.RewardOfferResp]
getRewardOffersHandler auth = withFlowHandlerAPI $ Domain.getRewardOffers auth
