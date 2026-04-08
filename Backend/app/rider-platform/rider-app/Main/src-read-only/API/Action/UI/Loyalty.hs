{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Loyalty
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.Loyalty
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Wallet.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API = (TokenAuth :> "wallet" :> "loyaltyInfo" :> Post ('[JSON]) Kernel.External.Wallet.Interface.Types.LoyaltyInfoResponse)

handler :: Environment.FlowServer API
handler = postWalletLoyaltyInfo

postWalletLoyaltyInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler Kernel.External.Wallet.Interface.Types.LoyaltyInfoResponse
  )
postWalletLoyaltyInfo a1 = withFlowHandlerAPI $ Domain.Action.UI.Loyalty.postWalletLoyaltyInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
