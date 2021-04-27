{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Product.Case where

import App.Types
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Models.Case as Case
import qualified Models.Product as Products
import qualified Models.ProductInstance as MPI
import qualified Storage.Queries.Location as Location
import Types.API.Case as API
import Types.Error

status ::
  Person.Person ->
  Id Case.Case ->
  FlowHandler StatusRes
status person caseId = withFlowHandlerAPI $ do
  case_ <- Case.findIdByPerson person caseId
  prodInstRes <- getProdInstances case_
  fromLocation <-
    fromMaybeM LocationNotFound
      =<< Location.findLocationById (Id $ case_ ^. #_fromLocationId)
  toLocation <-
    fromMaybeM LocationNotFound
      =<< Location.findLocationById (Id $ case_ ^. #_toLocationId)
  return $ StatusRes case_ prodInstRes fromLocation toLocation

list ::
  Person.Person ->
  Case.CaseType ->
  [Case.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler CaseListRes
list person caseType statuses mlimit moffset =
  withFlowHandlerAPI $
    Case.findAllByTypeAndStatuses (person ^. #_id) caseType statuses mlimit moffset
      >>= traverse mapProductInstance
  where
    mapProductInstance case_@Case.Case {..} = do
      prodInstRes <- getProdInstances case_
      fromLocation <- Location.findLocationById $ Id _fromLocationId
      toLocation <- Location.findLocationById $ Id _toLocationId
      return $ API.CaseRes case_ prodInstRes fromLocation toLocation

-- Core Utility functions are below

mkProdRes prodList prodInst =
  ProdInstRes
    { _id = prodInst ^. #_id,
      _caseId = prodInst ^. #_caseId,
      _productId = prodInst ^. #_productId,
      _personId = prodInst ^. #_personId,
      _shortId = prodInst ^. #_shortId,
      _entityType = prodInst ^. #_entityType,
      _entityId = prodInst ^. #_entityId,
      _quantity = prodInst ^. #_quantity,
      _price = prodInst ^. #_price,
      _status = prodInst ^. #_status,
      _startTime = prodInst ^. #_startTime,
      _endTime = prodInst ^. #_endTime,
      _validTill = prodInst ^. #_validTill,
      _fromLocation = prodInst ^. #_fromLocation,
      _toLocation = prodInst ^. #_toLocation,
      _organizationId = prodInst ^. #_organizationId,
      _parentId = prodInst ^. #_parentId,
      _udf1 = prodInst ^. #_udf1,
      _udf2 = prodInst ^. #_udf2,
      _udf3 = prodInst ^. #_udf3,
      _udf4 = prodInst ^. #_udf4,
      _udf5 = prodInst ^. #_udf5,
      _info = prodInst ^. #_info,
      _createdAt = prodInst ^. #_createdAt,
      _updatedAt = prodInst ^. #_updatedAt,
      _product = find (\x -> x ^. #_id == prodInst ^. #_productId) prodList
    }

getProdInstances :: Case.Case -> Flow [ProdInstRes]
getProdInstances case_@Case.Case {..} = do
  piList <- MPI.findAllByCaseId (Case._id case_)
  products <- Products.findAllByIds (PI._productId <$> piList)
  return $ mkProdRes products <$> piList
