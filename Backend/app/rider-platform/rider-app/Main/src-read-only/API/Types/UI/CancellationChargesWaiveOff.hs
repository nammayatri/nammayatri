{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.CancellationChargesWaiveOff where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common



data CancellationChargesWaiveOffRes
    = CancellationChargesWaiveOffRes {waivedOffAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
                                      waivedOffAmountWithCurrency :: Kernel.Prelude.Maybe Kernel.Types.Common.PriceAPIEntity,
                                      waivedOffSuccess :: Kernel.Prelude.Bool}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



