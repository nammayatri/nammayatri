{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Product.Case where

import App.Types
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as PI
import qualified Beckn.Types.Storage.Products as Products
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.ProductInstance as MPI
import qualified Storage.Queries.Products as Products
import Types.API.Case as API
import Types.Error
import Utils.Common

getStatus ::
  Person.Person ->
  Id Case.Case ->
  FlowHandler GetStatusRes
getStatus person caseId = withFlowHandlerAPI $ do
  case_ <- Case.findIdByPerson person caseId >>= fromMaybeM CaseDoesNotExist
  prodInstRes <- getProdInstances case_
  fromLocation <-
    fromMaybeM LocationNotFound
      =<< Location.findLocationById (case_.fromLocationId)
  toLocation <-
    fromMaybeM LocationNotFound
      =<< Location.findLocationById (case_.toLocationId)
  return $ GetStatusRes case_ prodInstRes fromLocation toLocation

list ::
  Person.Person ->
  Case.CaseType ->
  [Case.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler CaseListRes
list person caseType statuses mlimit moffset =
  withFlowHandlerAPI $
    Case.findAllByTypeAndStatuses (person.id) caseType statuses mlimit moffset
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
      status = prodInst.status,
      startTime = prodInst.startTime,
      endTime = prodInst.endTime,
      validTill = prodInst.validTill,
      fromLocation = prodInst.fromLocation,
      toLocation = prodInst.toLocation,
      organizationId = prodInst.organizationId,
      parentId = prodInst.parentId,
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
