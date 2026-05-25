module Storage.Queries.Transformers.IntegratedBPPConfig where

import qualified Data.Aeson as A
import qualified Domain.Types.IntegratedBPPConfig as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)

getProviderConfig :: (MonadFlow m) => A.Value -> m Domain.ProviderConfig
getProviderConfig configJSON =
  Kernel.Utils.JSON.valueToMaybe configJSON
    & fromMaybeM (InternalError ("Unable to decode IntegratedBPPConfigT.configJSON" <> show configJSON))

getProviderConfigJson :: Domain.ProviderConfig -> A.Value
getProviderConfigJson = \case
  Domain.DIRECT cfg -> toJSON cfg
  Domain.ONDC cfg -> toJSON cfg
