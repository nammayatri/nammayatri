{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Rewards
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Rewards
import qualified Control.Lens
import qualified Domain.Action.UI.Rewards
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RewardUnlock
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "rewards" :> Get ('[JSON]) [API.Types.UI.Rewards.RewardUnlockSummary] :<|> TokenAuth :> "rewards"
      :> Capture
           "unlockId"
           (Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock)
      :> "claim"
      :> Post
           ('[JSON])
           API.Types.UI.Rewards.ClaimCouponResp
      :<|> TokenAuth
      :> "rewards"
      :> Capture
           "unlockId"
           (Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock)
      :> "redeemed"
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getRewards :<|> postRewardsClaim :<|> postRewardsRedeemed

getRewards :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler [API.Types.UI.Rewards.RewardUnlockSummary])
getRewards a1 = withFlowHandlerAPI $ Domain.Action.UI.Rewards.getRewards (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postRewardsClaim ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock ->
    Environment.FlowHandler API.Types.UI.Rewards.ClaimCouponResp
  )
postRewardsClaim a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Rewards.postRewardsClaim (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postRewardsRedeemed ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.RewardUnlock.RewardUnlock ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postRewardsRedeemed a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Rewards.postRewardsRedeemed (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
