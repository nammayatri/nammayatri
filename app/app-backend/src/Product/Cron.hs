module Product.Cron where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (authenticate, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import Storage.Queries.Location as Loc
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import System.Environment
import qualified Types.API.Cron as API
import Types.API.ProductInstance
import Utils.Common (verifyToken)
import qualified Utils.Notifications as Notify

updateCases :: Maybe CronAuthKey -> API.ExpireCaseReq -> FlowHandler API.ExpireCaseRes
updateCases maybeAuth API.ExpireCaseReq {..} = withFlowHandler $ do
  authenticate maybeAuth
  cases <- Case.findAllExpiredByStatus [Case.NEW] from to
  traverse_
    ( \caseObj -> do
        let cId = Case._id caseObj
        Case.updateStatus cId Case.CLOSED
        ProductInstance.updateAllProductInstancesByCaseId cId ProductInstance.EXPIRED
        Notify.notifyOnExpiration caseObj
    )
    cases
  pure $ API.ExpireCaseRes $ length cases
