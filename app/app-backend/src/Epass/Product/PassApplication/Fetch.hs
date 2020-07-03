module Epass.Product.PassApplication.Fetch where

import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Location as BTL
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Beckn.Utils.Common
import Data.Aeson
import Data.List
import Data.Maybe
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Storage.Queries.Blacklist as Blacklist
import qualified Epass.Storage.Queries.Comment as Comment
import qualified Epass.Storage.Queries.Customer as Customer
import qualified Epass.Storage.Queries.Document as Document
import qualified Epass.Storage.Queries.EntityDocument as EntityDocument
import qualified Epass.Storage.Queries.EntityTag as EntityTag
import qualified Epass.Storage.Queries.Organization as Organization
import qualified Epass.Storage.Queries.PassApplication as DB
import qualified Epass.Storage.Queries.Tag as Tag
import qualified Epass.Types.API.PassApplication as API
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Common as Location (Location (..))
import qualified Epass.Types.Storage.Customer as Customer
import qualified Epass.Types.Storage.Document as Document
import qualified Epass.Types.Storage.EntityDocument as EntityDocument
import qualified Epass.Types.Storage.EntityTag as EntityTag
import Epass.Types.Storage.PassApplication
import qualified Epass.Types.Storage.Tag as Document
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as QC
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.ProductInstance as QCP
import qualified Storage.Queries.Products as QProd

listPassApplication ::
  RegToken ->
  Maybe Int ->
  Maybe Int ->
  [Int] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Int] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Case.CaseStatus] ->
  [OrganizationId] ->
  [Text] ->
  FlowHandler API.ListPassApplicationRes
listPassApplication regToken limitM offsetM fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType =
  withFlowHandler $ do
    token <- verifyToken regToken
    let entityId = scopeEntityAccess token
    fromLocationIds <- getLocationIds fPins fCities fStates fDists fWards
    toLocationIds <- getLocationIds toPins toCities toStates toDists toWards
    cases <- QC.findAllWithLimitOffsetWhere (_getLocationId <$> fromLocationIds) (_getLocationId <$> toLocationIds) [Case.PASSAPPLICATION] statuses passType limitM offsetM
    -- TODO: embed docs, comments, location and tags to the passapplication list response, once those are migrated
    caseApps <- traverse getCaseAppInfo cases
    return $ API.ListPassApplicationRes caseApps

getLocationIds :: [Int] -> [Text] -> [Text] -> [Text] -> [Text] -> L.Flow [LocationId]
getLocationIds pins cities states districts wards = do
  locs <- QLoc.findAllWithLimitOffsetWhere (show <$> pins) cities states districts wards Nothing Nothing
  return $ Location._id <$> locs

getPassAppInfo :: PassApplication -> L.Flow API.PassAppInfo
getPassAppInfo PassApplication {..} = do
  morg <- maybe (pure Nothing) Organization.findOrganizationById _OrganizationId
  mcustomer <- maybe (pure Nothing) Customer.findCustomerById _CustomerId
  let maybeCustId = Customer._id <$> mcustomer
  entityDocs <- EntityDocument.findAllByPassApplicationId _id
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> traverse Document.findById (DocumentId <$> docIds)
  entityTags <-
    maybe (pure []) (EntityTag.findAllByEntity "PASS_APPLICATION" . _getOrganizationId) _OrganizationId
  let tagIds = EntityTag._TagId <$> entityTags
  tags <- catMaybes <$> traverse Tag.findById (TagId <$> tagIds)
  comments <- Comment.findAllByCommentedOnEntity "PASS_APPLICATION" $ _getPassApplicationId _id
  isBlacklistedOrg <- maybe (pure False) (fmap isJust . Blacklist.findByOrgId) _OrganizationId
  let toLocation =
        Location
          { _type = fromMaybe BTL.PINCODE _toLocationType,
            _lat = _toLat,
            _long = _toLong,
            _ward = _toWard,
            _district = _toDistrict,
            _city = _toCity,
            _state = _toState,
            _country = _toCountry,
            _pincode = _toPincode,
            _address = _toAddress,
            _bound = _toBound
          }
  let fromLocation = case _fromLocationType of
        Just locType ->
          Just $
            Location
              { _type = locType,
                _lat = _fromLat,
                _long = _fromLong,
                _ward = _fromWard,
                _district = _fromDistrict,
                _city = _fromCity,
                _state = _fromState,
                _country = _fromCountry,
                _pincode = _fromPincode,
                _address = _fromAddress,
                _bound = _fromBound
              }
        Nothing -> Nothing
  pure
    API.PassAppInfo
      { _Customer = mcustomer,
        _Tags = tags,
        _Comments = comments,
        _Documents = docs,
        _Organization = morg,
        _isBlacklistedOrganization = isBlacklistedOrg,
        _isBlacklistedLocation = False,
        _fromLocation = fromLocation,
        _toLocation = toLocation,
        ..
      }

getCaseAppInfo :: Case.Case -> L.Flow API.CaseInfo
getCaseAppInfo Case.Case {..} = do
  morg <- maybe (pure Nothing) (Organization.findOrganizationById . OrganizationId) _udf2
  mcustomer <- maybe (pure Nothing) (QP.findById . PersonId) _requestor
  entityDocs <- EntityDocument.findAllByCaseId _id
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> traverse Document.findById (DocumentId <$> docIds)
  entityTags <- maybe (pure []) (EntityTag.findAllByEntity "PASS_APPLICATION") _udf2
  let tagIds = EntityTag._TagId <$> entityTags
  tags <- catMaybes <$> traverse Tag.findById (TagId <$> tagIds)
  comments <- Comment.findAllByCommentedOnEntity "PASS_APPLICATION" $ _getCaseId _id
  isBlacklistedOrg <-
    maybe (pure False) (fmap isJust . Blacklist.findByOrgId . OrganizationId) _udf2
  fromLocation <- QLoc.findLocationById $ LocationId _fromLocationId
  toLocation <- QLoc.findLocationById $ LocationId _toLocationId
  pure
    API.CaseInfo
      { _Customer = mcustomer,
        _Tags = tags,
        _Comments = comments,
        _Documents = docs,
        _Organization = morg,
        _isBlacklistedOrganization = isBlacklistedOrg,
        _isBlacklistedLocation = False,
        _fromLocation = fromLocation,
        _toLocation = toLocation,
        _passType = _udf1,
        _fromDate = _startTime,
        _toDate = _endTime,
        _purpose = Nothing,
        _AssignedTo = Nothing,
        _CreatedBy = _requestor,
        _count = _udf3,
        _approvedCount = _udf4,
        _TenantOrganizationId = Nothing,
        _remarks = _udf5,
        ..
      }

getPassApplicationById :: RegToken -> CaseId -> FlowHandler API.CaseInfo
getPassApplicationById regToken caseId = withFlowHandler $
  do
    verifyToken regToken
    case' :: Case.Case <- QC.findById caseId
    getCaseAppInfo case'

scopeEntityAccess :: RegistrationToken.RegistrationToken -> Maybe CustomerId
scopeEntityAccess RegistrationToken.RegistrationToken {..} =
  case _entityType of
    RegistrationToken.CUSTOMER -> Just (CustomerId _EntityId)
    RegistrationToken.USER -> Nothing
