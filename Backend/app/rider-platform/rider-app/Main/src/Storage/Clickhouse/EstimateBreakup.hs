module Storage.Clickhouse.EstimateBreakup where

import qualified Domain.Types.Estimate as DE
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common
import Kernel.Types.Id

data EstimateBreakupT f = EstimateBreakupT
  { id :: C f (Id DE.EstimateBreakup),
    estimateId :: C f (Id DE.Estimate),
    title :: C f Text,
    priceCurrency :: C f Currency,
    priceValue :: C f HighPrecMoney
  }
  deriving (Generic)

estimateBreakupTTable :: EstimateBreakupT (FieldModification EstimateBreakupT)
estimateBreakupTTable =
  EstimateBreakupT
    { id = "id",
      estimateId = "estimate_id",
      title = "title",
      priceCurrency = "price_currency",
      priceValue = "price_value"
    }

type EstimateBreakup = EstimateBreakupT Identity

$(TH.mkClickhouseInstances ''EstimateBreakupT)

findAllByEstimateIdT ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DE.Estimate ->
  m [EstimateBreakup]
findAllByEstimateIdT estimateId =
  CH.findAll $
    CH.select $
      CH.filter_
        ( \estimateBreakup _ ->
            estimateBreakup.estimateId CH.==. estimateId
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE estimateBreakupTTable)
