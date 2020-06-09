{-# LANGUAGE OverloadedLabels #-}

module Product.Case where

import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products
import Types.API.Case as API
import Utils.Common (verifyToken)

status ::
  Maybe RegToken ->
  CaseId ->
  FlowHandler StatusRes
status regToken caseId = withFlowHandler $ do
  verifyToken regToken
  case_ <- Case.findById caseId
  cpr <- CaseProduct.findAllByCaseId (Case._id case_)
  products <- Products.findAllByIds (CaseProduct._productId <$> cpr)
  fromLocation <-
    fromMaybeM500 "Could not find from location"
      =<< Location.findLocationById (LocationId $ case_ ^. #_fromLocationId)
  toLocation <-
    fromMaybeM500 "Could not find to location"
      =<< Location.findLocationById (LocationId $ case_ ^. #_toLocationId)
  return $ StatusRes case_ products fromLocation toLocation

list ::
  Maybe RegToken ->
  Case.CaseType ->
  [Case.CaseStatus] ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler ListRes
list regToken caseType statuses mlimit moffset = withFlowHandler $ do
  token <- verifyToken regToken
  person <-
    Person.findById (PersonId $ RegistrationToken._EntityId token)
      >>= fromMaybeM500 "Could not find user"
  Case.findAllByTypeAndStatuses (person ^. #_id) caseType statuses mlimit moffset
    >>= traverse mapCaseProduct
    >>= return . ListRes
  where
    mapCaseProduct case_@(Case.Case {..}) = do
      prds <- CaseProduct.findAllProductsByCaseId (_id)
      fromLocation <- Location.findLocationById $ LocationId _fromLocationId
      toLocation <- Location.findLocationById $ LocationId _toLocationId
      return $ API.CaseProduct case_ prds fromLocation toLocation
