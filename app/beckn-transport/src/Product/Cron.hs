{-# LANGUAGE OverloadedLabels #-}

module Product.Cron where

import App.Types
import Beckn.Types.App as BC
import Beckn.Types.Common
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Case as C
import qualified Beckn.Types.Storage.Person as PS
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Cron (authenticate)
import Beckn.Utils.Error
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (pi)
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Person as PSQ
import qualified Storage.Queries.ProductInstance as CPQ
import Types.API.Cron
import qualified Utils.Notifications as Notify

expireCases :: Maybe CronAuthKey -> ExpireCaseReq -> FlowHandler ExpireRes
expireCases maybeAuth ExpireCaseReq {..} = withFlowHandlerAPI $ do
  authenticate maybeAuth
  cases <- MC.findAllExpiredByStatus [C.NEW, C.CONFIRMED] C.RIDESEARCH from to
  productInstances <- CPQ.findAllByCaseIds (C._id <$> cases)
  updateCases cases
  updateProductInstances productInstances
  notifyTransporters cases productInstances
  pure $ ExpireRes $ length cases
  where
    updateCases cases = do
      mapM_ (\case_ -> C.validateStatusTransition (C._status case_) C.CLOSED & fromEitherM CaseInvalidStatus) cases
      MC.updateStatusByIds (C._id <$> cases) C.CLOSED
    updateProductInstances productInstances = do
      mapM_ (\pi -> PI.validateStatusTransition (PI._status pi) PI.EXPIRED & fromEitherM PIInvalidStatus) productInstances
      MPI.updateStatusByIds (PI._id <$> productInstances) PI.EXPIRED

notifyTransporters :: [C.Case] -> [PI.ProductInstance] -> Flow ()
notifyTransporters cases =
  traverse_
    ( \cp ->
        do
          admins <-
            PSQ.findAllByOrgIds [PS.ADMIN] $
              PI._organizationId <$> [cp]
          let caseObj = filter (\x -> PI._caseId cp == C._id x) cases
          case caseObj of
            [] -> pure ()
            x : _ -> Notify.notifyTransporterOnExpiration x admins
    )

expireProductInstances :: Maybe CronAuthKey -> FlowHandler ExpireRes
expireProductInstances maybeAuth = withFlowHandlerAPI $ do
  authenticate maybeAuth
  currTime <- getCurrentTime
  let timeToExpire = addUTCTime (-3 * 60 * 60) currTime
  piList <- MPI.findAllExpiredByStatus [PI.CONFIRMED, PI.INSTOCK] timeToExpire
  traverse_
    ( \pI ->
        case PI._type pI of
          C.RIDESEARCH -> do
            PI.validateStatusTransition (PI._status pI) PI.EXPIRED & fromEitherM PIInvalidStatus
            MPI.updateStatus (PI._id pI) PI.EXPIRED
          C.RIDEORDER -> do
            cs <- MC.findById (PI._caseId pI)
            C.validateStatusTransition (cs ^. #_status) C.CLOSED & fromEitherM CaseInvalidStatus
            MC.updateStatus (PI._caseId pI) C.CLOSED
            PI.validateStatusTransition (PI._status pI) PI.EXPIRED & fromEitherM PIInvalidStatus
            MPI.updateStatus (PI._id pI) PI.EXPIRED
          _ -> return ()
    )
    piList
  pure $ ExpireRes $ length piList
