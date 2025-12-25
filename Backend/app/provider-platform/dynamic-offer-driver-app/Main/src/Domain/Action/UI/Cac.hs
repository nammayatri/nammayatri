module Domain.Action.UI.Cac where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import SharedLogic.Cac

postDriverGetUiConfigs ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant, Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Data.Aeson.Object ->
  Environment.Flow Data.Aeson.Object
postDriverGetUiConfigs (_, _, merchantOpCityId) toss tenant context = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  if useCACConfig
    then getFrontendConfigs merchantOpCityId toss tenant context <&> fromMaybe Data.Aeson.KeyMap.empty
    else return Data.Aeson.KeyMap.empty
