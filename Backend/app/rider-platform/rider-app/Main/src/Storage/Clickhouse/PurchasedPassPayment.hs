module Storage.Clickhouse.PurchasedPassPayment where

import Data.Time
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id

data PurchasedPassPaymentT f = PurchasedPassPaymentT
  { id :: C f (Id DPPP.PurchasedPassPayment),
    passCode :: C f Text,
    amount :: C f HighPrecMoney,
    benefitValue :: C f (Maybe HighPrecMoney),
    status :: C f Text,
    merchantOperatingCityId :: C f (Id DMOC.MerchantOperatingCity),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

purchasedPassPaymentTTable :: PurchasedPassPaymentT (FieldModification PurchasedPassPaymentT)
purchasedPassPaymentTTable =
  PurchasedPassPaymentT
    { id = "id",
      passCode = "pass_code",
      amount = "amount",
      benefitValue = "benefit_value",
      status = "status",
      merchantOperatingCityId = "merchant_operating_city_id",
      createdAt = "created_at"
    }

type PurchasedPassPayment = PurchasedPassPaymentT Identity

deriving instance Show PurchasedPassPayment

$(TH.mkClickhouseInstances ''PurchasedPassPaymentT 'NO_SELECT_MODIFIER)

-- Aggregated pass payment metrics result
data PassPaymentMetrics = PassPaymentMetrics
  { metricsPassCode :: Text,
    metricsPassCount :: Int,
    metricsTotalAmount :: HighPrecMoney,
    metricsTotalBenefitValue :: HighPrecMoney
  }
  deriving (Generic, Show)

-- Get aggregated pass payment metrics for a date grouped by pass code.
-- Counts payments whose status is in [Active, PreBooked, Expired]; cost = amount + benefitValue.
getPassPaymentMetricsByDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMOC.MerchantOperatingCity ->
  Day ->
  m [PassPaymentMetrics]
getPassPaymentMetricsByDateRange merchantOpCityId targetDate = do
  let istOffset = 19800 :: NominalDiffTime -- 5 hours 30 minutes in seconds
      startOfDayUTC = addUTCTime (negate istOffset) (UTCTime targetDate 0)
      endOfDayUTC = addUTCTime (86400 - istOffset) (UTCTime targetDate 0)
      eligibleStatuses = ["Active", "PreBooked", "Expired"] :: [Text]
  results <-
    CH.findAll $
      CH.select_
        ( \p ->
            CH.groupBy p.passCode $ \passCode ->
              let cnt = CH.count_ p.id
                  totalAmt = CH.sum_ p.amount
                  -- SUM ignores NULL rows in ClickHouse, so no need for
                  -- ifNull(benefit_value, 0) in SQL — that mismatched the
                  -- column's Float64 with the DSL's HighPrecMoney
                  -- string-serialized literal '0.0' (NO_COMMON_TYPE error).
                  totalBenefit = CH.sum_ p.benefitValue
               in (passCode, cnt, totalAmt, totalBenefit)
        )
        $ CH.filter_
          ( \p ->
              p.merchantOperatingCityId CH.==. merchantOpCityId
                CH.&&. CH.in_ p.status eligibleStatuses
                CH.&&. p.createdAt >=. startOfDayUTC
                CH.&&. p.createdAt <. endOfDayUTC
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE purchasedPassPaymentTTable)
  pure $
    map
      ( \(code, cnt, totalAmt, totalBenefit) ->
          PassPaymentMetrics
            { metricsPassCode = code,
              metricsPassCount = cnt,
              metricsTotalAmount = totalAmt,
              metricsTotalBenefitValue = fromMaybe 0 totalBenefit
            }
      )
      results
