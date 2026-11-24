-- | Kept as an empty stub after haskell-cac removal for backward compatibility:
-- older rider apps may still call /v2/getUiConfigs; dropping the route
-- would 404 them. Real CAC configs are gone — always return {}.
module Domain.Action.UI.Cac where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id

postGetUiConfigs ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Data.Aeson.Object ->
  Environment.Flow Data.Aeson.Object
postGetUiConfigs _ _ = pure Data.Aeson.KeyMap.empty
