module Storage.Queries.Transformers.TransporterConfig
  ( fallBackVersionInText,
    parseFieldM,
    parseFieldWithDefaultM,
    parseAnalyticsConfig,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Version
import Kernel.Utils.Common

fallbackVersion :: Version
fallbackVersion =
  Version
    { major = 9999,
      minor = 9999,
      maintenance = 9999,
      preRelease = Nothing,
      build = Nothing
    }

fallBackVersionInText :: Text
fallBackVersionInText = versionToText fallbackVersion

parseFieldM :: (Monad m, Log m, FromJSON a) => Text -> Text -> Text -> Maybe A.Value -> m (Maybe a)
parseFieldM table field entityId = \case
  Just val -> case A.fromJSON val of
    A.Success x -> pure $ Just x
    A.Error err -> do
      logError $ "Entity field parsing failed for table: " <> table <> "; field: " <> field <> "; entityId: " <> entityId <> "; err: " <> show err <> "; default values will be used"
      pure Nothing
  Nothing -> pure Nothing

parseFieldWithDefaultM :: forall m a. (Monad m, Log m, FromJSON a) => Text -> Text -> Text -> a -> Maybe A.Value -> m a
parseFieldWithDefaultM table field entityId def = (fromMaybe def <$>) . parseFieldM table field entityId

parseAnalyticsConfig :: (Monad m, Log m) => Text -> Maybe A.Value -> m DTC.AnalyticsConfig
parseAnalyticsConfig merchantOperatingCityId mbVal = do
  let def =
        DTC.AnalyticsConfig
          { allowCacheDriverFlowStatus = Just False
          }
  parseFieldWithDefaultM "transporterConfig" "analyticsConfig" merchantOperatingCityId def mbVal
