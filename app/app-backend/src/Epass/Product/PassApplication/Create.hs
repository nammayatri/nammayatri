module Epass.Product.PassApplication.Create where

import qualified Beckn.Types.Common as BTC
import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Loc
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Aeson
import qualified Data.Text as T
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Product.Organization as PO
import qualified Epass.Storage.Queries.Customer as Customer
import qualified Epass.Storage.Queries.CustomerDetail as CustomerDetail
import qualified Epass.Storage.Queries.Organization as QO
import qualified Epass.Storage.Queries.PassApplication as DB
import qualified Epass.Types.API.PassApplication as API
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Common as Location (Location (..))
import qualified Epass.Types.Storage.Customer as Customer
import qualified Epass.Types.Storage.CustomerDetail as CD
import Epass.Types.Storage.PassApplication
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as QC
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.Person as QP
import qualified Test.RandomStrings as RS
import qualified Utils.Common as UC

createPassApplication ::
  RegToken -> API.CreatePassApplicationReq -> FlowHandler API.PassApplicationRes'
createPassApplication regToken req@API.CreatePassApplicationReq {..} = withFlowHandler $ do
  token <- UC.verifyToken regToken
  caseInfo <-
    case _type of
      SELF -> selfFlow token req
      SPONSOR -> sponsorFlow token req
      BULKSPONSOR -> bulkSponsorFlow token req
  QC.create caseInfo
  QC.findById (Case._id caseInfo)
    >>= return . API.PassApplicationRes'

bulkSponsorFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow Case.Case
bulkSponsorFlow token req@API.CreatePassApplicationReq {..} = do
  when
    (isNothing _OrganizationId)
    (L.throwException $ err400 {errBody = "OrganizationId cannot be empty"})
  let organizationId = fromJust _OrganizationId
  when
    (isNothing _count || _count == Just 0)
    (L.throwException $ err400 {errBody = "Count cannot be 0"})
  QO.findOrganizationById organizationId
    >>= fromMaybeM400 "Organization does not exists"
  getCaseInfo token req Nothing

selfFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow Case.Case
selfFlow token req@API.CreatePassApplicationReq {..} = do
  when
    (isNothing _CustomerId)
    (L.throwException $ err400 {errBody = "CustomerId cannot be empty"})
  when
    (isNothing _travellerName || isNothing _travellerIDType || isNothing _travellerID)
    (L.throwException $ err400 {errBody = "travellerName, travellerIDType and travellerID cannot be empty"})
  let customerId = fromJust _CustomerId
      travellerID = fromJust _travellerID
      travellerIDType = mapIdType' <$> _travellerIDType
  when
    (customerId /= (PersonId (RegistrationToken._EntityId token)))
    (L.throwException $ err400 {errBody = "CustomerId mismatch"})
  QP.update customerId Nothing _travellerName Nothing Nothing travellerIDType _travellerID
  getCaseInfo token req _CustomerId

sponsorFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow Case.Case
sponsorFlow token req@API.CreatePassApplicationReq {..} = do
  when
    (isNothing _travellerName || isNothing _travellerIDType || isNothing _travellerID)
    (L.throwException $ err400 {errBody = "travellerName, travellerIDType and travellerID cannot be empty"})
  let travellerName = fromJust _travellerName
      travellerID = fromJust _travellerID
      travellerIDType = mapIdType' $ fromJust _travellerIDType
  QP.findByIdentifier travellerIDType travellerID
    >>= \case
      Just cust -> getCaseInfo token req (Just $ SP._id cust)
      Nothing -> do
        currTime <- getCurrentTimeUTC
        pId <- L.generateGUID
        QP.create
          SP.Person
            { _id = PersonId pId,
              _firstName = _travellerName,
              _middleName = Nothing,
              _lastName = Nothing,
              _fullName = Nothing,
              _role = SP.USER,
              _gender = SP.UNKNOWN,
              _identifierType = travellerIDType,
              _email = Nothing,
              _mobileNumber = getMobileNumberM travellerIDType _travellerID,
              _mobileCountryCode = Nothing,
              _identifier = _travellerID,
              _rating = Nothing,
              _verified = False,
              _status = SP.INACTIVE,
              _udf1 = Nothing,
              _udf2 = Nothing,
              _deviceToken = Nothing,
              _organizationId = Nothing,
              _locationId = Nothing,
              _description = Nothing,
              _createdAt = currTime,
              _updatedAt = currTime
            }
        getCaseInfo token req (Just $ PersonId pId)
  where
    getMobileNumberM SP.MOBILENUMBER tId = tId
    getMobileNumberM _ _ = Nothing

getPassType :: PassApplicationType -> PassType
getPassType SELF = INDIVIDUAL
getPassType SPONSOR = INDIVIDUAL
getPassType BULKSPONSOR = ORGANIZATION

getCount :: PassApplicationType -> Maybe Int -> L.Flow Int
getCount SELF _ = return 1
getCount SPONSOR _ = return 1
getCount BULKSPONSOR (Just c) = return c
getCount BULKSPONSOR Nothing = L.throwException $ err400 {errBody = "Count cannot be null"}

mapIdType MOBILE = CD.MOBILENUMBER
mapIdType AADHAAR = CD.AADHAAR

mapIdType' MOBILE = SP.MOBILENUMBER
mapIdType' AADHAAR = SP.AADHAAR

getLocation :: API.CreatePassApplicationReq -> L.Flow (Loc.Location, Loc.Location)
getLocation API.CreatePassApplicationReq {..} = do
  toId <- BTC.generateGUID
  fromId <- BTC.generateGUID
  currTime <- getCurrentTimeUTC
  let fromLocation =
        Loc.Location
          { _id = toId,
            _locationType = maybe Loc.PINCODE Location._type _fromLocation,
            _lat = Location._lat =<< _fromLocation,
            _long = Location._long =<< _fromLocation,
            _ward = Location._ward =<< _fromLocation,
            _district = Location._district =<< _fromLocation,
            _city = Location._city =<< _fromLocation,
            _state = Location._state =<< _fromLocation,
            _country = Location._country =<< _fromLocation,
            _pincode = show <$> (Location._pincode =<< _fromLocation),
            _address = Location._address =<< _fromLocation,
            _bound = Nothing, -- Location._bound =<< _fromLocation
            _createdAt = currTime,
            _updatedAt = currTime
          }
  let toLocation =
        Loc.Location
          { _id = fromId,
            _locationType = Loc.PINCODE, -- (Location._type _toLocation)
            _lat = Location._lat _toLocation,
            _long = Location._long _toLocation,
            _ward = Location._ward _toLocation,
            _district = Location._district _toLocation,
            _city = Location._city _toLocation,
            _state = Location._state _toLocation,
            _country = Location._country _toLocation,
            _pincode = show <$> (Location._pincode _toLocation),
            _address = Location._address _toLocation,
            _bound = Nothing, -- (Location._bound _toLocation)
            _createdAt = currTime,
            _updatedAt = currTime
          }
  return (fromLocation, toLocation)

getCaseInfo :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> Maybe PersonId -> L.Flow Case.Case
getCaseInfo token req@API.CreatePassApplicationReq {..} mCustId = do
  id <- BTC.generateGUID
  (fromLoc, toLoc) <- getLocation req
  QL.create fromLoc
  QL.create toLoc
  customer <- QP.findById (PersonId $ SR._EntityId token)
  let customerOrgId = SP._organizationId =<< customer
  currTime <- getCurrentTimeUTC
  count <- getCount _type _count
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  let toLocationId = _getLocationId $ Loc._id toLoc
      fromLocationId = _getLocationId $ Loc._id fromLoc
  return $
    Case.Case
      { _id = id,
        _name = Nothing,
        _description = Nothing,
        _shortId = T.pack shortId,
        _industry = Case.GOVT,
        _type = Case.PASSAPPLICATION,
        _exchangeType = Case.ORDER,
        _status = Case.NEW,
        _startTime = _fromDate,
        _endTime = Just _toDate,
        _validTill = _fromDate,
        _provider = Nothing,
        _providerType = Just Case.GOVTADMIN,
        _requestor = _getPersonId <$> mCustId,
        _requestorType = Just Case.CONSUMER,
        _parentCaseId = Nothing,
        _fromLocationId = fromLocationId,
        _toLocationId = toLocationId,
        _udf1 = Just $ show $ getPassType _type, -- passtype
        _udf2 = customerOrgId, -- customer org id
        _udf3 = (show <$> _count), -- count
        _udf4 = Nothing, -- approved count
        _udf5 = Nothing, -- remarks
        _info = Nothing,
        _createdAt = currTime,
        _updatedAt = currTime
      }

createCustomer :: Text -> L.Flow Customer.Customer
createCustomer name = do
  id <- generateGUID
  Customer.create =<< (getCust id)
  Customer.findCustomerById id
    >>= fromMaybeM500 "Unable to create customer"
  where
    getCust id = do
      now <- getCurrentTimeUTC
      return $
        Customer.Customer
          { _id = id,
            _name = Just name,
            _OrganizationId = Nothing,
            _TenantOrganizationId = Nothing,
            _verified = False,
            _role = Customer.INDIVIDUAL,
            _info = Nothing,
            _createdAt = now,
            _updatedAt = now
          }
