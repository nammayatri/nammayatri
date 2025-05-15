module Storage.Queries.Transformers.ProviderDetails where

import qualified Data.Aeson as A
import qualified Domain.Types.Route as Domain
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)

getProviderDetailsMaybe :: (MonadFlow m) => Maybe A.Value -> m (Maybe Domain.ProviderDetails)
getProviderDetailsMaybe (Just val) = Just <$> getProviderDetails val
getProviderDetailsMaybe Nothing = pure Nothing

getProviderDetails :: (MonadFlow m) => A.Value -> m Domain.ProviderDetails
getProviderDetails configJSON = Kernel.Utils.JSON.valueToMaybe configJSON & fromMaybeM (InternalError "Unable to decode ProviderDetailsT.configJSON")

getProviderDetailsJson :: Domain.ProviderDetails -> A.Value
getProviderDetailsJson = \case
  Domain.ONDC cfg -> toJSON cfg
