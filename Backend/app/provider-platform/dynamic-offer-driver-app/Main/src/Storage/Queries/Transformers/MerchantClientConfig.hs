module Storage.Queries.Transformers.MerchantClientConfig where

import qualified Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.MerchantClientConfig as Domain
import Kernel.Prelude as P
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

getConfigJSON :: Domain.ClientServiceConfig -> Data.Aeson.Value
getConfigJSON (Domain.ClientFCMServiceConfig fcmConfig) = toJSON fcmConfig

getServiceName :: Domain.ClientServiceConfig -> Domain.ClientService
getServiceName (Domain.ClientFCMServiceConfig _) = Domain.ClientFCMService

mkServiceConfig :: (MonadThrow m, Log m) => Data.Aeson.Value -> Domain.ClientService -> m Domain.ClientServiceConfig
mkServiceConfig configJSON serviceName = either (\err -> throwError $ InternalError ("Unable to decode MerchantServiceConfigT.configJSON: " <> show configJSON <> " Error:" <> err)) return $ case serviceName of
  Domain.ClientFCMService -> Domain.ClientFCMServiceConfig <$> eitherValue configJSON
  where
    eitherValue :: FromJSON a => A.Value -> Either Text a
    eitherValue value = case A.fromJSON value of
      A.Success a -> Right a
      A.Error err -> Left $ T.pack err
