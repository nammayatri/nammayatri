module Product.Cron where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as C
import qualified Beckn.Types.Storage.Person as PS
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.Products as P
import Beckn.Utils.Common (authenticate, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Models.Case as MC
import qualified Models.ProductInstance as MCP
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.Person as PSQ
import qualified Storage.Queries.ProductInstance as CPQ
import qualified Storage.Queries.Products as PQ
import Types.API.Cron
import qualified Utils.Notifications as Notify

expire :: Maybe CronAuthKey -> ExpireCaseReq -> FlowHandler ExpireCaseRes
expire maybeAuth ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- CQ.findAllExpiredByStatus [C.NEW] C.RIDESEARCH from to
  productInstances <- CPQ.findAllByCaseIds (C._id <$> cases)
  MC.updateStatusByIds (C._id <$> cases) C.CLOSED
  MCP.updateStatusByIds (PI._id <$> productInstances) PI.EXPIRED
  notifyTransporters cases productInstances
  pure $ ExpireCaseRes $ length cases

notifyTransporters :: [C.Case] -> [PI.ProductInstance] -> L.Flow ()
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
