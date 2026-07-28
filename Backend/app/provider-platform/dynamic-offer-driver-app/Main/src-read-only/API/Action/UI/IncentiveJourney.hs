{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.IncentiveJourney
  ( API,
    handler,
  )
where

import qualified API.Types.UI.IncentiveJourney
import qualified Control.Lens
import qualified Data.Text
import qualified Domain.Action.UI.IncentiveJourney
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

type API =
  ( TokenAuth :> "incentive" :> "journey" :> "list" :> QueryParam "active" Kernel.Prelude.Bool :> QueryParam "date" Data.Text.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.UI.IncentiveJourney.IncentiveJourneyListRes
      :<|> TokenAuth
      :> "incentive"
      :> "journey"
      :> "history"
      :> QueryParam
           "date"
           Data.Text.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> Get
           ('[JSON])
           API.Types.UI.IncentiveJourney.IncentiveJourneyHistoryRes
  )

handler :: Environment.FlowServer API
handler = getIncentiveJourneyList :<|> getIncentiveJourneyHistory

getIncentiveJourneyList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Bool) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Environment.FlowHandler API.Types.UI.IncentiveJourney.IncentiveJourneyListRes
  )
getIncentiveJourneyList a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.IncentiveJourney.getIncentiveJourneyList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1

getIncentiveJourneyHistory ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Data.Text.Text) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
    Environment.FlowHandler API.Types.UI.IncentiveJourney.IncentiveJourneyHistoryRes
  )
getIncentiveJourneyHistory a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.IncentiveJourney.getIncentiveJourneyHistory (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a4) a3 a2 a1
