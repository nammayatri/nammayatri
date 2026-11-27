-- | Kept as an empty stub after haskell-cac removal for backward compatibility:
-- older driver apps may still call /driver/getUiConfigs; dropping the route
-- would 404 them. Real CAC configs are gone — always return {}.
module Domain.Action.UI.Cac where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id

postDriverGetUiConfigs ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  Data.Aeson.Object ->
  Environment.Flow Data.Aeson.Object
postDriverGetUiConfigs _ _ = pure Data.Aeson.KeyMap.empty
