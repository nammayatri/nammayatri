module Storage.Clickhouse.Estimate where

import qualified Domain.Types.Estimate as DE
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Kernel.Utils.Common

-- | Slim Clickhouse projection of the postgres @estimate@ table — only the
-- columns we read on the breakup-fallback path. The @breakup_list_json@
-- column mirrors the new postgres column populated by
-- 'Storage.Queries.Transformers.Estimate.encodeEstimateBreakupList'.
data EstimateT f = EstimateT
  { id :: C f (Id DE.Estimate),
    breakupListJson :: C f (Maybe Text),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show Estimate

estimateTTable :: EstimateT (FieldModification EstimateT)
estimateTTable =
  EstimateT
    { id = "id",
      breakupListJson = "estimate_breakup_list_json",
      createdAt = "created_at"
    }

type Estimate = EstimateT Identity

$(TH.mkClickhouseInstances ''EstimateT 'SELECT_FINAL_MODIFIER)

findById ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DE.Estimate ->
  UTCTime ->
  m (Maybe Estimate)
findById estimateId createdAt = do
  res <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \estimate ->
              estimate.id CH.==. estimateId
                CH.&&. estimate.createdAt >=. addUTCTime (-1800) createdAt
                CH.&&. estimate.createdAt <=. createdAt
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE estimateTTable)
  pure (listToMaybe res)
