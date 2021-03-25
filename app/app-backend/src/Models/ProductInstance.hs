module Models.ProductInstance where

import App.Types
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.Products (Products)
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude
import qualified Storage.Queries.ProductInstance as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

create :: ProductInstance -> Flow ()
create prdInst = do
  result <- Q.create prdInst
  checkDBError result

-- | Validate and update ProductInstance status
updateStatus :: Id ProductInstance -> ProductInstanceStatus -> Flow ()
updateStatus piid status = do
  validatePIStatusChange status piid
  result <- Q.updateStatus piid status
  checkDBError result

-- | Bulk validate and update Case's ProductInstances statuses
updateAllProductInstancesByCaseId :: Id Case.Case -> ProductInstanceStatus -> Flow ()
updateAllProductInstancesByCaseId caseId status = do
  validatePIStatusesChange status caseId
  result <- Q.updateAllProductInstancesByCaseId caseId status
  checkDBError result

updateMultiple :: Id ProductInstance -> ProductInstance -> Flow ()
updateMultiple piid prdInst = do
  validatePIStatusChange (_status prdInst) piid
  result <- Q.updateMultiple piid prdInst
  checkDBError result

-- | Find Product Instance by id
findById :: Id ProductInstance -> Flow ProductInstance
findById caseProductId = do
  result <- Q.findById caseProductId
  checkDBErrorOrEmpty result ProductInstanceNotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: Id Case.Case -> Flow [ProductInstance]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId caseId
  checkDBError result

-- | Find Product Instance by Product Id
findByProductId :: Id Products -> Flow ProductInstance
findByProductId pId = do
  result <- Q.findByProductId pId
  checkDBErrorOrEmpty result ProductInstanceNotFound

-- | Get ProductInstance and validate its status change
validatePIStatusChange :: ProductInstanceStatus -> Id ProductInstance -> Flow ()
validatePIStatusChange newStatus productInstanceId = do
  cp <- findById productInstanceId
  validateStatusChange newStatus cp

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange :: ProductInstanceStatus -> Id Case.Case -> Flow ()
validatePIStatusesChange newStatus caseId = do
  cps <- findAllByCaseId caseId
  validatePIStatusesChange' newStatus cps

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange' :: ProductInstanceStatus -> [ProductInstance] -> Flow ()
validatePIStatusesChange' newStatus =
  mapM_ (validateStatusChange newStatus)

-- | Validate status change and return appropriate DomainError
validateStatusChange :: ProductInstanceStatus -> ProductInstance -> Flow ()
validateStatusChange newStatus caseProduct =
  case validateStatusTransition (_status caseProduct) newStatus of
    Left msg -> throwErrorWithInfo404 ProductInstanceInvalidStatus msg
    _ -> pure ()

listAllProductInstanceWithOffset :: Integer -> Integer -> ListById -> [ProductInstanceStatus] -> [Case.CaseType] -> Flow [ProductInstance]
listAllProductInstanceWithOffset limit offset piid stats csTypes = do
  result <- Q.listAllProductInstanceWithOffset limit offset piid stats csTypes
  checkDBError result

listAllProductInstance :: ListById -> [ProductInstanceStatus] -> Flow [ProductInstance]
listAllProductInstance piid status = do
  result <- Q.listAllProductInstance piid status
  checkDBError result

listAllProductInstanceByPerson :: Person.Person -> ListById -> [ProductInstanceStatus] -> Flow [ProductInstance]
listAllProductInstanceByPerson person piid status = do
  result <- Q.listAllProductInstanceByPerson person piid status
  checkDBError result

findAllByParentId :: Maybe (Id ProductInstance) -> Flow [ProductInstance]
findAllByParentId piid = do
  result <- Q.findAllByParentId piid
  checkDBError result

findByParentIdType :: Maybe (Id ProductInstance) -> Case.CaseType -> Flow ProductInstance
findByParentIdType mparentId csType = do
  result <- Q.findByParentIdType mparentId csType
  checkDBErrorOrEmpty result ProductInstanceNotFound

findAllByPerson :: Id Person.Person -> Flow [ProductInstance]
findAllByPerson perId = do
  result <- Q.findAllByPerson perId
  checkDBError result

findAllExpiredByStatus :: [ProductInstanceStatus] -> UTCTime -> Flow [ProductInstance]
findAllExpiredByStatus statuses expiryTime = do
  result <- Q.findAllExpiredByStatus statuses expiryTime
  checkDBError result
