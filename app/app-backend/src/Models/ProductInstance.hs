module Models.ProductInstance where

import App.Types
import Beckn.Types.App
import Beckn.Types.Error
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance
import Beckn.Utils.Common
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
updateStatus :: ProductInstanceId -> ProductInstanceStatus -> Flow ()
updateStatus id status = do
  validatePIStatusChange status id
  result <- Q.updateStatus id status
  checkDBError result

-- | Bulk validate and update Case's ProductInstances statuses
updateAllProductInstancesByCaseId :: CaseId -> ProductInstanceStatus -> Flow ()
updateAllProductInstancesByCaseId caseId status = do
  validatePIStatusesChange status caseId
  result <- Q.updateAllProductInstancesByCaseId caseId status
  checkDBError result

updateMultiple :: ProductInstanceId -> ProductInstance -> Flow ()
updateMultiple id prdInst = do
  validatePIStatusChange (_status prdInst) id
  result <- Q.updateMultiple id prdInst
  checkDBError result

-- | Find Product Instance by id
findById :: ProductInstanceId -> Flow ProductInstance
findById caseProductId = do
  result <- Q.findById caseProductId
  checkDBErrorOrEmpty result $ ProductInstanceErr ProductInstanceNotFound

-- | Find Product Instances by Case Id
findAllByCaseId :: CaseId -> Flow [ProductInstance]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId caseId
  checkDBError result

-- | Find Product Instance by Product Id
findByProductId :: ProductsId -> Flow ProductInstance
findByProductId pId = do
  result <- Q.findByProductId pId
  checkDBErrorOrEmpty result $ ProductInstanceErr ProductInstanceNotFound

-- | Get ProductInstance and validate its status change
validatePIStatusChange :: ProductInstanceStatus -> ProductInstanceId -> Flow ()
validatePIStatusChange newStatus productInstanceId = do
  cp <- findById productInstanceId
  validateStatusChange newStatus cp

-- | Bulk validation of ProductInstance statuses change
validatePIStatusesChange :: ProductInstanceStatus -> CaseId -> Flow ()
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
    Left msg -> throwDomainError $ ProductInstanceErr $ ProductInstanceStatusTransitionErr $ ErrorMsg msg
    _ -> pure ()

listAllProductInstanceWithOffset :: Integer -> Integer -> ListById -> [ProductInstanceStatus] -> [Case.CaseType] -> Flow [ProductInstance]
listAllProductInstanceWithOffset limit offset id stats csTypes = do
  result <- Q.listAllProductInstanceWithOffset limit offset id stats csTypes
  checkDBError result

listAllProductInstance :: ListById -> [ProductInstanceStatus] -> Flow [ProductInstance]
listAllProductInstance id status = do
  result <- Q.listAllProductInstance id status
  checkDBError result

listAllProductInstanceByPerson :: Person.Person -> ListById -> [ProductInstanceStatus] -> Flow [ProductInstance]
listAllProductInstanceByPerson person id status = do
  result <- Q.listAllProductInstanceByPerson person id status
  checkDBError result

findAllByParentId :: Maybe ProductInstanceId -> Flow [ProductInstance]
findAllByParentId id = do
  result <- Q.findAllByParentId id
  checkDBError result

findByParentIdType :: Maybe ProductInstanceId -> Case.CaseType -> Flow ProductInstance
findByParentIdType mparentId csType = do
  result <- Q.findByParentIdType mparentId csType
  checkDBErrorOrEmpty result $ ProductInstanceErr ProductInstanceNotFound

findAllByPerson :: PersonId -> Flow [ProductInstance]
findAllByPerson perId = do
  result <- Q.findAllByPerson perId
  checkDBError result
