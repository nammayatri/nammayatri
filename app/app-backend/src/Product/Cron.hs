module Product.Cron where

import App.Types
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common (authenticate, withFlowHandlerAPI)
import Data.Time (addUTCTime)
import EulerHS.Prelude
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Types.API.Cron as API
import qualified Utils.Metrics as Metrics
import qualified Utils.Notifications as Notify

updateCases :: Maybe CronAuthKey -> API.ExpireCaseReq -> FlowHandler API.ExpireRes
updateCases maybeAuth API.ExpireCaseReq {..} = withFlowHandlerAPI $ do
  authenticate maybeAuth
  cases <- MC.findAllExpiredByStatus [Case.NEW] from to
  traverse_
    ( \caseObj -> do
        let cId = Case._id caseObj
        Metrics.incrementCaseCount Case.CLOSED (Case._type caseObj)
        MC.updateStatus cId Case.CLOSED
        MPI.updateAllProductInstancesByCaseId cId PI.EXPIRED
        Notify.notifyOnExpiration caseObj
    )
    cases
  pure $ API.ExpireRes $ length cases

expireProductInstances :: Maybe CronAuthKey -> FlowHandler API.ExpireRes
expireProductInstances maybeAuth = withFlowHandlerAPI $ do
  authenticate maybeAuth
  currTime <- getCurrentTime
  let timeToExpire = addUTCTime (-3 * 60 * 60) currTime
  piList <- MPI.findAllExpiredByStatus [PI.CONFIRMED, PI.INSTOCK] timeToExpire
  traverse_
    ( \pI ->
        case PI._type pI of
          Case.RIDESEARCH -> MPI.updateStatus (PI._id pI) PI.EXPIRED
          Case.RIDEORDER -> do
            cs <- MC.findById (PI._caseId pI)
            MC.updateStatus (PI._caseId pI) Case.CLOSED
            MPI.updateStatus (PI._id pI) PI.EXPIRED
            Notify.notifyOnExpiration cs
          _ -> return ()
    )
    piList
  pure $ API.ExpireRes $ length piList
