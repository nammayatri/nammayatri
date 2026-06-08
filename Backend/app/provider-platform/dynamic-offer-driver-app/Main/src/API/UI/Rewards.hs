{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Rewards where

import qualified Domain.Action.UI.Rewards as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.RewardOffer as DReward
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  "rewards"
    :> ( "offers"
           :> TokenAuth
           :> Get '[JSON] [Domain.RewardOfferResp]
           :<|> "offers"
             :> Capture "offerId" (Id DReward.RewardOffer)
             :> "progress"
             :> TokenAuth
             :> Get '[JSON] Domain.RewardOfferProgressResp
       )

handler :: FlowServer API
handler =
  getRewardOffersHandler
    :<|> getRewardOfferProgressHandler

getRewardOffersHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler [Domain.RewardOfferResp]
getRewardOffersHandler auth = withFlowHandlerAPI $ Domain.getRewardOffers auth

getRewardOfferProgressHandler :: Id DReward.RewardOffer -> (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> FlowHandler Domain.RewardOfferProgressResp
getRewardOfferProgressHandler offerId auth = withFlowHandlerAPI $ Domain.getRewardOfferProgress auth offerId
