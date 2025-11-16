{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Penalty
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Penalty
import qualified Control.Lens
import qualified Domain.Action.UI.Penalty
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

type API = (TokenAuth :> "penalty" :> "check" :> ReqBody ('[JSON]) API.Types.UI.Penalty.PenaltyCheckReq :> Post ('[JSON]) API.Types.UI.Penalty.PenaltyCheckRes)

handler :: Environment.FlowServer API
handler = postPenaltyCheck

postPenaltyCheck ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Penalty.PenaltyCheckReq ->
    Environment.FlowHandler API.Types.UI.Penalty.PenaltyCheckRes
  )
postPenaltyCheck a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Penalty.postPenaltyCheck (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
