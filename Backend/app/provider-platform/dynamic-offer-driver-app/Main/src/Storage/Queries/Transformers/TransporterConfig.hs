module Storage.Queries.Transformers.TransporterConfig
  ( fallBackVersionInText,
    parseFieldM,
    parseFieldWithDefaultM,
    parseAnalyticsConfig,
    parseDriverWalletConfig,
    parseSubscriptionConfig,
    parseTaxConfig,
  )
where

import qualified Data.Aeson as A
import Domain.Types.TransporterConfig hiding (SubscriptionConfig (..))
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Version
import Storage.Queries.Transformers.SubscriptionConfigParser (parseSubscriptionConfig)
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
          { weekStartMode = 3,
            earningsWindowSize = 7,
            allowCacheDriverFlowStatus = False,
            enableFleetOperatorDashboardAnalytics = False,
            maxOnlineDurationDays = 10,
            onlineDurationCalculateFrom = Nothing,
            useDbForEarningAndMetrics = False
          }
  parseFieldWithDefaultM "transporterConfig" "analyticsConfig" merchantOperatingCityId def parseAnalyticsConfigWithDefault mbVal

$(mkFieldParserWithDefault ''DriverWalletConfig)

parseDriverWalletConfig :: (Monad m, Log m) => Text -> Maybe A.Value -> m DriverWalletConfig
parseDriverWalletConfig merchantOperatingCityId mbVal = do
  let def =
        DriverWalletConfig
          { enableDriverWallet = False,
            driverWalletPayoutThreshold = 0,
            gstPercentage = 0.0,
            enableWalletPayout = False,
            enableWalletTopup = False,
            maxWalletPayoutsPerDay = Nothing,
            minWalletAmountForCashRides = Nothing,
            minimumWalletPayoutAmount = 0,
            payoutCutOffDays = 7,
            payoutFee = Nothing
          }
  parseFieldWithDefaultM "transporterConfig" "driverWalletConfig" merchantOperatingCityId def parseDriverWalletConfigWithDefault mbVal

$(mkFieldParserWithDefault ''TaxConfig)

parseTaxConfig :: (Monad m, Log m) => Text -> Maybe A.Value -> m TaxConfig
parseTaxConfig merchantOperatingCityId mbVal = do
  let def =
        TaxConfig
          { rideGst = GstBreakup {cgstPercentage = Nothing, sgstPercentage = Nothing, igstPercentage = Nothing},
            securityDepositGst = Nothing,
            defaultTdsRate = Nothing,
            subscriptionTdsRate = Nothing,
            invalidPanTdsRate = 0
          }
  parseFieldWithDefaultM "transporterConfig" "taxConfig" merchantOperatingCityId def parseTaxConfigWithDefault mbVal
