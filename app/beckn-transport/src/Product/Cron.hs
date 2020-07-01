{-# LANGUAGE OverloadedLabels #-}

module Product.Cron where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as C
import qualified Beckn.Types.Storage.Person as PS
import qualified Beckn.Types.Storage.ProductInstance as CP
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
  cases <- CQ.findAllExpiredByStatus [C.NEW] C.RIDEBOOK from to
  traverse_
    ( \caseObj -> do
        let cId = C._id caseObj
        productInstances <- CPQ.findAllByCaseIds (C._id <$> cases)
        products <- PQ.findAllById (CP._productId <$> productInstances)
        MC.updateStatus cId C.CLOSED
        MCP.updateAllProductInstancesByCaseId cId CP.EXPIRED
        notifyTransporters cases productInstances products
    )
    cases
  pure $ ExpireCaseRes $ length cases

notifyTransporters :: [C.Case] -> [CP.ProductInstance] -> [P.Products] -> L.Flow ()
notifyTransporters cases productInstances products =
  traverse_
    ( \cp -> do
        let filteredProducts = filter (\x -> CP._productId cp == P._id x) products
        admins <- PSQ.findAllByOrgIds [PS.ADMIN] $ P._organizationId <$> filteredProducts
        let caseObj = filter (\x -> CP._caseId cp == C._id x) cases
        case caseObj of
          [] -> pure ()
          x : _ -> Notify.notifyTransporterOnExpiration x admins
    )
    productInstances
