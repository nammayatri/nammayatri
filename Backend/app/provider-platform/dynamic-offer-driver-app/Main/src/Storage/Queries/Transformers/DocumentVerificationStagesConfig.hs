module Storage.Queries.Transformers.DocumentVerificationStagesConfig where

import qualified Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationStagesConfig
import Kernel.Prelude
import Kernel.Utils.Common (MonadFlow, logError)

mkMediaJSON :: Maybe [Domain.Types.DocumentVerificationStagesConfig.MediaInfo] -> Maybe Data.Aeson.Value
mkMediaJSON = fmap Data.Aeson.toJSON

getMediaFromJSON :: MonadFlow m => Maybe Data.Aeson.Value -> m (Maybe [Domain.Types.DocumentVerificationStagesConfig.MediaInfo])
getMediaFromJSON Nothing = pure Nothing
getMediaFromJSON (Just val) = case Data.Aeson.fromJSON val of
  Data.Aeson.Success x -> pure $ Just x
  Data.Aeson.Error err -> do
    logError $ "Unable to decode DocumentVerificationStagesConfigT.mediaJSON: " <> show err <> " value: " <> show val
    pure Nothing

mkGuidelinesJSON :: Maybe [Domain.Types.DocumentVerificationConfig.ImageInfo] -> Maybe Data.Aeson.Value
mkGuidelinesJSON = fmap Data.Aeson.toJSON

getGuidelinesFromJSON :: MonadFlow m => Maybe Data.Aeson.Value -> m (Maybe [Domain.Types.DocumentVerificationConfig.ImageInfo])
getGuidelinesFromJSON Nothing = pure Nothing
getGuidelinesFromJSON (Just val) = case Data.Aeson.fromJSON val of
  Data.Aeson.Success x -> pure $ Just x
  Data.Aeson.Error err -> do
    logError $ "Unable to decode DocumentVerificationStagesConfigT.guidelinesJSON: " <> show err <> " value: " <> show val
    pure Nothing
