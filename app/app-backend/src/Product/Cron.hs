module Product.Cron where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common (authenticate, withFlowHandler)
import EulerHS.Prelude
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Types.API.Cron as API
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

updateCases :: Maybe CronAuthKey -> API.ExpireCaseReq -> FlowHandler API.ExpireCaseRes
updateCases maybeAuth API.ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- MC.findAllExpiredByStatus [Case.NEW] from to
  traverse_
    ( \caseObj -> do
        let cId = Case._id caseObj
        Metrics.incrementCaseCount Case.CLOSED (Case._type caseObj)
        MC.updateStatus cId Case.CLOSED
        MPI.updateAllProductInstancesByCaseId cId ProductInstance.EXPIRED
        Notify.notifyOnExpiration caseObj
    )
    cases
  pure $ API.ExpireCaseRes $ length cases
