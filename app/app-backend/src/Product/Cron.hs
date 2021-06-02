{-# LANGUAGE OverloadedLabels #-}

module Product.Cron where

import App.Types
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Cron (authenticate)
import Beckn.Utils.Error
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (pi)
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
        let cId = Case.id caseObj
        Metrics.incrementCaseCount Case.CLOSED (Case._type caseObj)
        Case.validateStatusTransition (caseObj ^. #status) Case.CLOSED & fromEitherM CaseInvalidStatus
        MC.updateStatus cId Case.CLOSED
        updateAllProductInstancesByCaseId cId
        Notify.notifyOnExpiration caseObj
    )
    cases
  pure $ API.ExpireRes $ length cases
  where
    updateAllProductInstancesByCaseId cId = do
      casePIs <- MPI.findAllByCaseId cId
      mapM_ (\pi -> PI.validateStatusTransition (pi ^. #status) PI.EXPIRED & fromEitherM PIInvalidStatus) casePIs
      MPI.updateAllProductInstancesByCaseId cId PI.EXPIRED

expireProductInstances :: Maybe CronAuthKey -> FlowHandler API.ExpireRes
expireProductInstances maybeAuth = withFlowHandlerAPI $ do
  authenticate maybeAuth
  currTime <- getCurrentTime
  let timeToExpire = addUTCTime (-3 * 60 * 60) currTime
  piList <- MPI.findAllExpiredByStatus [PI.CONFIRMED, PI.INSTOCK] timeToExpire
  traverse_
    ( \pI ->
        case PI._type pI of
          Case.RIDESEARCH -> do
            PI.validateStatusTransition (PI.status pI) PI.EXPIRED & fromEitherM PIInvalidStatus
            MPI.updateStatus (PI.id pI) PI.EXPIRED
          Case.RIDEORDER -> do
            cs <- MC.findById (PI.caseId pI)
            Case.validateStatusTransition (cs ^. #status) Case.CLOSED & fromEitherM CaseInvalidStatus
            MC.updateStatus (PI.caseId pI) Case.CLOSED
            PI.validateStatusTransition (PI.status pI) PI.EXPIRED & fromEitherM PIInvalidStatus
            MPI.updateStatus (PI.id pI) PI.EXPIRED
            Notify.notifyOnExpiration cs
          _ -> return ()
    )
    piList
  pure $ API.ExpireRes $ length piList
