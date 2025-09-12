module Storage.Queries.Transformers.TransporterConfig
  ( fallBackVersionInText,
    parseFieldM,
    parseFieldWithDefaultM,
    parseAnalyticsConfig,
  )
where

import qualified Data.Aeson as A
import Domain.Types.TransporterConfig
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Version
import Tools.FieldParse as FieldParse

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

$(mkFieldParserWithDefault ''AnalyticsConfig)

parseAnalyticsConfig :: (Monad m, Log m) => Text -> Maybe A.Value -> m AnalyticsConfig
parseAnalyticsConfig merchantOperatingCityId mbVal = do
  let def =
        AnalyticsConfig
          { allowCacheDriverFlowStatus = False
          }
  parseFieldWithDefaultM "transporterConfig" "analyticsConfig" merchantOperatingCityId def parseAnalyticsConfigWithDefault mbVal
