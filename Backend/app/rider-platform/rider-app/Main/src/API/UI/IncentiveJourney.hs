module API.UI.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.IncentiveJourney as DIncentiveJourney
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.IncentiveJourney.API.UI.IncentiveJourney as IA
import qualified Lib.IncentiveJourney.Common.UI.IncentiveJourney as Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth
import Tools.FlowHandling (withFlowHandlerAPIPersonId)

type API = "incentive" :> "journey" :> TokenAuth :> IA.IncentiveJourneyAPI

handler :: FlowServer API
handler authInfo =
  getIncentiveJourneyList authInfo
    :<|> getIncentiveJourneyHistory authInfo

getIncentiveJourneyList ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler Common.IncentiveJourneyListRes
getIncentiveJourneyList (personId, merchantId) mbLimit mbOffset =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    DIncentiveJourney.getIncentiveJourneyList (personId, merchantId) mbLimit mbOffset

getIncentiveJourneyHistory ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler Common.IncentiveJourneyHistoryRes
getIncentiveJourneyHistory (personId, merchantId) mbDate mbLimit mbOffset =
  withFlowHandlerAPIPersonId personId . withPersonIdLogTag personId $
    DIncentiveJourney.getIncentiveJourneyHistory (personId, merchantId) mbDate mbLimit mbOffset
