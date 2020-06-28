module Models.Case where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.Case
import Beckn.Utils.Common (fromDBError)
import Data.Text
import qualified EulerHS.Language as L
import qualified Storage.Queries.Case as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

updateStatus :: CaseId -> CaseStatus -> FlowResult ()
updateStatus id status = do
  result <- Q.updateStatus id status
  fromDBError result

updateStatusAndUdfs :: CaseId -> CaseStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> FlowResult ()
updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5 = do
  result <- Q.updateStatusAndUdfs id status udf1 udf2 udf3 udf4 udf5
  fromDBError result

updateStatusByIds :: [CaseId] -> CaseStatus -> FlowResult ()
updateStatusByIds ids status = do
  result <- Q.updateStatusByIds id status udf1 udf2 udf3 udf4 udf5
  fromDBError result
