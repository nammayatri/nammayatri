{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.IncentiveJourney as Domain
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.API.UI.IncentiveJourney as IA
import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = "incentive" :> "journey" :> TokenAuth :> IA.IncentiveJourneyAPI

handler :: FlowServer API
handler = externalHandler
  where
    externalHandler (personId, merchantId, merchantOpCityId) =
      getIncentiveJourneyList (Just personId, merchantId, merchantOpCityId)
        :<|> getIncentiveJourneyHistory (Just personId, merchantId, merchantOpCityId)

getIncentiveJourneyList ::
  (Maybe (Id SP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler Common.IncentiveJourneyListRes
getIncentiveJourneyList authParams mbLimit mbOffset =
  withFlowHandlerAPI $ Domain.getIncentiveJourneyList authParams mbLimit mbOffset

getIncentiveJourneyHistory ::
  (Maybe (Id SP.Person), Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler Common.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory authParams mbDate mbLimit mbOffset =
  withFlowHandlerAPI $ Domain.getIncentiveJourneyHistory authParams mbDate mbLimit mbOffset
