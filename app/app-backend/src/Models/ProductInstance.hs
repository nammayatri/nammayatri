module Models.ProductInstance where

import App.Types
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.Products (Products)
import Data.Time
import EulerHS.Prelude
import qualified Storage.Queries.ProductInstance as Q
import Types.Error
import Utils.Common

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

create :: ProductInstance -> Flow ()
create prdInst = do
  Q.createFlow prdInst

-- | Validate and update ProductInstance status
updateStatus :: Id ProductInstance -> ProductInstanceStatus -> Flow ()
updateStatus piid status = do
  Q.updateStatus piid status

-- | Bulk validate and update Case's ProductInstances statuses
updateAllProductInstancesByCaseId :: Id Case.Case -> ProductInstanceStatus -> Flow ()
updateAllProductInstancesByCaseId caseId status = do
  Q.updateAllProductInstancesByCaseId caseId status

updateMultiple :: Id ProductInstance -> ProductInstance -> Flow ()
updateMultiple piid prdInst = do
  Q.updateMultipleFlow piid prdInst

-- | Find Product Instance by id
findById :: Id ProductInstance -> Flow ProductInstance
findById caseProductId = do
  Q.findById caseProductId >>= fromMaybeM PINotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: Id Case.Case -> Flow [ProductInstance]
findAllByCaseId caseId = do
  Q.findAllByCaseId caseId

-- | Find Product Instance by Product Id
findByProductId :: Id Products -> Flow ProductInstance
findByProductId pId = do
  Q.findByProductId pId >>= fromMaybeM PINotFound

listAllProductInstanceWithOffset :: Integer -> Integer -> ListById -> [ProductInstanceStatus] -> [Case.CaseType] -> Flow [ProductInstance]
listAllProductInstanceWithOffset limit offset piid stats csTypes = do
  Q.listAllProductInstanceWithOffset limit offset piid stats csTypes

listAllProductInstance :: ListById -> [ProductInstanceStatus] -> Flow [ProductInstance]
listAllProductInstance piid status = do
  Q.listAllProductInstance piid status

listAllProductInstanceByPerson :: Person.Person -> ListById -> [ProductInstanceStatus] -> Flow [ProductInstance]
listAllProductInstanceByPerson person piid status = do
  Q.listAllProductInstanceByPerson person piid status

findAllByParentId :: Id ProductInstance -> Flow [ProductInstance]
findAllByParentId piid = do
  Q.findAllByParentId piid

findByParentIdType :: Id ProductInstance -> Case.CaseType -> Flow ProductInstance
findByParentIdType mparentId csType = do
  Q.findByParentIdType mparentId csType >>= fromMaybeM PINotFound

findAllByPerson :: Id Person.Person -> Flow [ProductInstance]
findAllByPerson perId = do
  Q.findAllByPerson perId

findAllExpiredByStatus :: [ProductInstanceStatus] -> UTCTime -> Flow [ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  Q.findAllExpiredByStatus statuses expiryTime
