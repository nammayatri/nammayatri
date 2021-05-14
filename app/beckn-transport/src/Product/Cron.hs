module Product.Cron where

import App.Types
import Beckn.Types.App as BC
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as C
import qualified Beckn.Types.Storage.Person as PS
import qualified Beckn.Types.Storage.ProductInstance as PI
import Data.Time (addUTCTime)
import EulerHS.Prelude
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Person as PSQ
import qualified Storage.Queries.ProductInstance as CPQ
import Types.API.Cron
import Utils.Common (authenticate, withFlowHandlerAPI)
import qualified Utils.Notifications as Notify

expireCases :: Maybe CronAuthKey -> ExpireCaseReq -> FlowHandler ExpireRes
expireCases maybeAuth ExpireCaseReq {..} = withFlowHandlerAPI $ do
  authenticate maybeAuth
  cases <- MC.findAllExpiredByStatus [C.NEW, C.CONFIRMED] C.RIDESEARCH from to
  productInstances <- CPQ.findAllByCaseIds (C._id <$> cases)
  MC.updateStatusByIds (C._id <$> cases) C.CLOSED
  MPI.updateStatusByIds (PI._id <$> productInstances) PI.EXPIRED
  notifyTransporters cases productInstances
  pure $ ExpireRes $ length cases

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
          C.RIDESEARCH -> MPI.updateStatus (PI._id pI) PI.EXPIRED
          C.RIDEORDER -> do
            MC.updateStatus (PI._caseId pI) C.CLOSED
            MPI.updateStatus (PI._id pI) PI.EXPIRED
          _ -> return ()
    )
    piList
  pure $ ExpireRes $ length piList
