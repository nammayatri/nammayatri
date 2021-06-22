module Models.ProductInstance where

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

create :: HasFlowDBEnv m r => ProductInstance -> m ()
create prdInst = do
  Q.createFlow prdInst

-- | Validate and update ProductInstance status
updateStatus :: HasFlowDBEnv m r => Id ProductInstance -> ProductInstanceStatus -> m ()
updateStatus piid status = do
  Q.updateStatus piid status

-- | Bulk validate and update Case's ProductInstances statuses
updateAllProductInstancesByCaseId :: HasFlowDBEnv m r => Id Case.Case -> ProductInstanceStatus -> m ()
updateAllProductInstancesByCaseId caseId status = do
  Q.updateAllProductInstancesByCaseId caseId status

updateMultiple :: HasFlowDBEnv m r => Id ProductInstance -> ProductInstance -> m ()
updateMultiple piid prdInst = do
  Q.updateMultipleFlow piid prdInst

-- | Find Product Instance by id
findById :: HasFlowDBEnv m r => Id ProductInstance -> m ProductInstance
findById caseProductId = do
  Q.findById caseProductId >>= fromMaybeM PINotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: HasFlowDBEnv m r => Id Case.Case -> m [ProductInstance]
findAllByCaseId caseId = do
  Q.findAllByCaseId caseId

-- | Find Product Instance by Product Id
findByProductId :: HasFlowDBEnv m r => Id Products -> m ProductInstance
findByProductId pId = do
  Q.findByProductId pId >>= fromMaybeM PINotFound

listAllProductInstanceWithOffset :: HasFlowDBEnv m r => Integer -> Integer -> ListById -> [ProductInstanceStatus] -> [Case.CaseType] -> m [ProductInstance]
listAllProductInstanceWithOffset limit offset piid stats csTypes = do
  Q.listAllProductInstanceWithOffset limit offset piid stats csTypes

listAllProductInstance :: HasFlowDBEnv m r => ListById -> [ProductInstanceStatus] -> m [ProductInstance]
listAllProductInstance piid status = do
  Q.listAllProductInstance piid status

listAllProductInstanceByPerson :: HasFlowDBEnv m r => Person.Person -> ListById -> [ProductInstanceStatus] -> m [ProductInstance]
listAllProductInstanceByPerson person piid status = do
  Q.listAllProductInstanceByPerson person piid status

findAllByParentId :: HasFlowDBEnv m r => Id ProductInstance -> m [ProductInstance]
findAllByParentId piid = do
  Q.findAllByParentId piid

findByParentIdType :: HasFlowDBEnv m r => Id ProductInstance -> Case.CaseType -> m ProductInstance
findByParentIdType mparentId csType = do
  Q.findByParentIdType mparentId csType >>= fromMaybeM PINotFound

findAllByPerson :: HasFlowDBEnv m r => Id Person.Person -> m [ProductInstance]
findAllByPerson perId = do
  Q.findAllByPerson perId

findAllExpiredByStatus :: HasFlowDBEnv m r => [ProductInstanceStatus] -> UTCTime -> m [ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  Q.findAllExpiredByStatus statuses expiryTime
