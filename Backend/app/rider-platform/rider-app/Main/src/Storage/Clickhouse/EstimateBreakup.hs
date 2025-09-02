module Storage.Clickhouse.EstimateBreakup where

import qualified Domain.Types.Estimate as DE
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common

data EstimateBreakupT f = EstimateBreakupT
  { id :: C f (Id DE.EstimateBreakup),
    estimateId :: C f (Id DE.Estimate),
    title :: C f Text,
    priceCurrency :: C f Currency,
    priceValue :: C f HighPrecMoney,
    date :: C f (Maybe UTCTime)
  }
  deriving (Generic)

estimateBreakupTTable :: EstimateBreakupT (FieldModification EstimateBreakupT)
estimateBreakupTTable =
  EstimateBreakupT
    { id = "id",
      estimateId = "estimate_id",
      title = "title",
      priceCurrency = "price_currency",
      priceValue = "price_value",
      date = "date"
    }

type EstimateBreakup = EstimateBreakupT Identity

$(TH.mkClickhouseInstances ''EstimateBreakupT 'SELECT_FINAL_MODIFIER)

addMinutesToUTCTime :: Int -> UTCTime -> UTCTime
addMinutesToUTCTime minutes = addUTCTime (fromIntegral $ minutes * 60)

findAllByEstimateIdT ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DE.Estimate ->
  UTCTime ->
  m [EstimateBreakup]
findAllByEstimateIdT estimateId createdAt =
  CH.findAll $
    CH.select $
      CH.filter_
        ( \estimateBreakup ->
            estimateBreakup.estimateId CH.==. estimateId
              CH.&&. estimateBreakup.date >=. Just (addMinutesToUTCTime (-10) createdAt)
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE estimateBreakupTTable)
