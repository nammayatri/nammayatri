{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Product.Case where

import App.Types
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.ProductInstance as MPI
import qualified Storage.Queries.Products as Products
import qualified Storage.Queries.SearchReqLocation as Location
import Types.API.Case as API
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Products as Products
import Utils.Common

getStatus ::
  Id Person.Person ->
  Id Case.Case ->
  FlowHandler GetStatusRes
getStatus personId caseId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  case_ <- Case.findIdByPersonId personId caseId >>= fromMaybeM CaseDoesNotExist
  prodInstRes <- getProdInstances case_
  fromLocation <-
    fromMaybeM LocationNotFound
      =<< Location.findLocationById (case_.fromLocationId)
  toLocation <-
    fromMaybeM LocationNotFound
      =<< Location.findLocationById (case_.toLocationId)
  return $ GetStatusRes case_ prodInstRes fromLocation toLocation

list ::
  Id Person.Person ->
  Case.CaseType ->
  [Case.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler CaseListRes
list personId caseType statuses mlimit moffset =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    Case.findAllByTypeAndStatuses personId caseType statuses mlimit moffset
      >>= traverse mapProductInstance
  where
    mapProductInstance case_@Case.Case {..} = do
      prodInstRes <- getProdInstances case_
      fromLocation <- Location.findLocationById fromLocationId
      toLocation <- Location.findLocationById toLocationId
      return $ API.CaseRes case_ prodInstRes fromLocation toLocation

-- Core Utility functions are below
mkProdRes :: [Products.Products] -> PI.ProductInstance -> ProdInstRes
mkProdRes prodList prodInst =
  ProdInstRes
    { id = prodInst.id,
      caseId = prodInst.caseId,
      productId = prodInst.productId,
      personId = prodInst.personId,
      shortId = prodInst.shortId,
      entityType = prodInst.entityType,
      entityId = prodInst.entityId,
      quantity = prodInst.quantity,
      price = prodInst.price,
      actualPrice = prodInst.actualPrice,
      status = prodInst.status,
      startTime = prodInst.startTime,
      endTime = prodInst.endTime,
      validTill = prodInst.validTill,
      fromLocation = prodInst.fromLocation,
      toLocation = prodInst.toLocation,
      organizationId = prodInst.organizationId,
      parentId = prodInst.parentId,
      chargableDistance = prodInst.chargableDistance,
      udf1 = prodInst.udf1,
      udf2 = prodInst.udf2,
      udf3 = prodInst.udf3,
      udf4 = prodInst.udf4,
      udf5 = prodInst.udf5,
      info = prodInst.info,
      createdAt = prodInst.createdAt,
      updatedAt = prodInst.updatedAt,
      product = find (\x -> x.id == prodInst.productId) prodList
    }

getProdInstances :: DBFlow m r => Case.Case -> m [ProdInstRes]
getProdInstances case_@Case.Case {..} = do
  piList <- MPI.findAllByCaseId (Case.id case_)
  products <- Products.findAllByIds (PI.productId <$> piList)
  return $ mkProdRes products <$> sortBy sortPI piList
  where
    sortPI pi1 pi2 =
      let dist1 :: Maybe Double = readMaybe . T.unpack =<< (pi1.udf1)
          dist2 :: Maybe Double = readMaybe . T.unpack =<< (pi2.udf1)
       in case (dist1, dist2) of
            (Just d1, Just d2) -> compare d1 d2
            (Just _, _) -> GT
            (_, Just _) -> LT
            (_, _) -> EQ
