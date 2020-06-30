module Models.CaseProduct where

import Beckn.Types.App
import Beckn.Types.Error
import Beckn.Types.Storage.CaseProduct
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.CaseProduct as Q

-- The layer between Storage.Queries and our business logic
-- Here we should perform validations of all kinds
-- and return types like Either Error a
-- If all checks are ok, call Queries functions and do not send
-- any possible database errors outside of this module.
-- Convert it to DomainError with a proper description

updateStatus :: ProductsId -> CaseProductStatus -> FlowDomainResult ()
updateStatus id status = runExceptT $ do
  validateCPStatusChange status id
  ExceptT $ do
    result <- Q.updateStatus id status
    fromDBError result

updateAllCaseProductsByCaseId :: CaseId -> CaseProductStatus -> FlowDomainResult ()
updateAllCaseProductsByCaseId caseId status = runExceptT $ do
  validateCPSStatusesChange status caseId
  ExceptT $ do
    result <- Q.updateAllCaseProductsByCaseId caseId status
    fromDBError result

findById :: CaseProductId -> FlowDomainResult CaseProduct
findById caseProductId = do
  result <- Q.findById' caseProductId
  fromDBErrorOrEmpty (CaseProductErr CaseProductNotFound) result

findAllByCaseId :: CaseId -> FlowDomainResult [CaseProduct]
findAllByCaseId caseId = do
  result <- Q.findAllByCaseId' caseId
  fromDBError result

findByProductId :: ProductsId -> FlowDomainResult CaseProduct
findByProductId pId = do
  result <- Q.findByProductId' pId
  fromDBErrorOrEmpty (CaseProductErr CaseProductNotFound) result

validateCPStatusChange :: CaseProductStatus -> ProductsId -> ExceptT DomainError L.Flow ()
validateCPStatusChange newStatus caseId = do
  cp <- ExceptT $ findByProductId caseId
  case validateStatusChange newStatus cp of
    Left err -> ExceptT . pure . Left $ err
    Right _ -> pure ()

validateCPSStatusesChange :: CaseProductStatus -> CaseId -> ExceptT DomainError L.Flow ()
validateCPSStatusesChange newStatus caseId = do
  cps <- ExceptT $ findAllByCaseId caseId
  case sequence $ fmap (validateStatusChange newStatus) cps of
    Left err -> ExceptT . pure . Left $ err
    Right _ -> pure ()

validateStatusChange :: CaseProductStatus -> CaseProduct -> DomainResult ()
validateStatusChange newStatus caseProduct =
  case validateStatusTransition (_status caseProduct) newStatus of
    Left msg -> Left $ CaseProductErr $ CaseProductStatusTransitionErr msg
    _ -> Right ()
