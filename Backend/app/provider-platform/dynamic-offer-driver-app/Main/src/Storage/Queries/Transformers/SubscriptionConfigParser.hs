module Storage.Queries.Transformers.SubscriptionConfigParser
  ( parseSubscriptionConfig,
  )
where

import qualified Data.Aeson as A
import Domain.Types.TransporterConfig (SubscriptionConfig (..))
import Kernel.Prelude
import Kernel.Types.Common
import Tools.FieldParse as FieldParse

$(mkFieldParserWithDefault ''SubscriptionConfig)

parseSubscriptionConfig :: (Monad m, Log m) => Text -> Maybe A.Value -> m SubscriptionConfig
parseSubscriptionConfig merchantOperatingCityId mbVal = do
  let def = SubscriptionConfig {prepaidSubscriptionThreshold = Nothing, fleetPrepaidSubscriptionThreshold = Nothing, tdsRate = Nothing}
  parseFieldWithDefaultM "transporterConfig" "subscriptionConfig" merchantOperatingCityId def parseSubscriptionConfigWithDefault mbVal
