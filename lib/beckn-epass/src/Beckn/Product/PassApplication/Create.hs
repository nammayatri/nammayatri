module Beckn.Product.PassApplication.Create where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Customer        as Customer
import qualified Beckn.Storage.Queries.CustomerDetail  as CustomerDetail
import qualified Beckn.Storage.Queries.Organization    as QO
import qualified Beckn.Storage.Queries.PassApplication as DB
import qualified Beckn.Types.API.PassApplication       as API
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Beckn.Types.Storage.Customer          as Customer
import qualified Beckn.Types.Storage.CustomerDetail    as CD
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Extra
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

createPassApplication ::
  Maybe Text -> API.CreatePassApplicationReq -> FlowHandler API.PassApplicationRes
createPassApplication regToken req@API.CreatePassApplicationReq{..} = withFlowHandler $ do
  token <- verifyToken regToken

  passAppInfo <-
    case _type of
      SELF        -> selfFlow token req
      SPONSOR     -> sponsorFlow token req
      BULKSPONSOR -> bulkSponsorFlow token req

  DB.create passAppInfo
  DB.findById (_id passAppInfo)
    >>= fromMaybeM500 "Could not create PassApplication"
    >>= return . API.PassApplicationRes

bulkSponsorFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow PassApplication
bulkSponsorFlow token req@API.CreatePassApplicationReq{..} = do
  when (isNothing _OrganizationId)
    (L.throwException $ err400 {errBody = "OrganizationId cannot be empty"})
  let organizationId = fromJust _OrganizationId

  when (isNothing _count || _count == Just 0)
    (L.throwException $ err400 {errBody = "Count cannot be 0"})

  QO.findOrganizationById organizationId
    >>= fromMaybeM400 "Organization does not exists"
  getPassAppInfo token req Nothing

selfFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow PassApplication
selfFlow token req@API.CreatePassApplicationReq{..} = do
  when (isNothing _CustomerId)
    (L.throwException $ err400 {errBody = "CustomerId cannot be empty"})
  when (isNothing _travellerName || isNothing _travellerIDType || isNothing _travellerID)
    (L.throwException $ err400 {errBody = "travellerName, travellerIDType and travellerID cannot be empty"})
  let
    customerId = fromJust _CustomerId
    travellerID = fromJust _travellerID
    travellerIDType = mapIdType $ fromJust _travellerIDType

  when (customerId /= (CustomerId (RegistrationToken._EntityId token)))
    (L.throwException $ err400 {errBody = "CustomerId mismatch"})

  Customer.updateDetails customerId _travellerName _OrganizationId
  CustomerDetail.createIfNotExists customerId travellerIDType travellerID
  getPassAppInfo token req _CustomerId

sponsorFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow PassApplication
sponsorFlow token req@API.CreatePassApplicationReq{..} = do
  when (isNothing _travellerName || isNothing _travellerIDType || isNothing _travellerID)
    (L.throwException $ err400 {errBody = "travellerName, travellerIDType and travellerID cannot be empty"})

  let
    travellerName = fromJust _travellerName
    travellerID = fromJust _travellerID
    travellerIDType = mapIdType $ fromJust _travellerIDType
  CustomerDetail.findByIdentifier travellerIDType travellerID
   >>= \case
    Just cd -> getPassAppInfo token req (Just $ CD._CustomerId cd)
    Nothing -> do
      customer <- createCustomer travellerName
      let customerId = Customer._id customer
      CustomerDetail.createIfNotExists customerId travellerIDType travellerID
      getPassAppInfo token req (Just customerId)

getPassAppInfo :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> Maybe CustomerId -> L.Flow PassApplication
getPassAppInfo token API.CreatePassApplicationReq{..} mCustId = do
  id <- generateGUID
  currTime <- getCurrTime
  count <- getCount _type _count
  return $ PassApplication
          { _id = id
          , _CustomerId = mCustId
          , _passType = getPassType _type
          , _fromLocationType = Location._type <$> _fromLocation
          , _fromLat = join (Location._lat <$> _fromLocation)
          , _fromLong = join (Location._long <$> _fromLocation)
          , _fromWard = join (Location._ward <$> _fromLocation)
          , _fromDistrict = join (Location._district <$> _fromLocation)
          , _fromCity = join (Location._city <$> _fromLocation)
          , _fromState = join (Location._state <$> _fromLocation)
          , _fromCountry = join (Location._country <$> _fromLocation)
          , _fromPincode = join (Location._pincode <$> _fromLocation)
          , _fromAddress = join (Location._address <$> _fromLocation)
          , _fromBound = join (Location._bound <$> _fromLocation)
          , _toLocationType = Just $ Location._type _toLocation
          , _toLat = Location._lat _toLocation
          , _toLong = Location._long _toLocation
          , _toWard = Location._ward _toLocation
          , _toDistrict = Location._district _toLocation
          , _toCity = Location._city _toLocation
          , _toState = Location._state _toLocation
          , _toCountry = Location._country _toLocation
          , _toPincode = Location._pincode _toLocation
          , _toAddress = Location._address _toLocation
          , _toBound = Location._bound _toLocation
          , _createdAt = currTime
          , _updatedAt = currTime
          , _status = PENDING
          , _CreatedBy = CustomerId (RegistrationToken._EntityId token)
          , _AssignedTo = UserId "admin" -- TODO: fix this
          , _count = count
          , _approvedCount = 0
          , _remarks = ""
          , _info = ""
          , ..
          }

getPassType :: PassApplicationType -> PassType
getPassType SELF        = INDIVIDUAL
getPassType SPONSOR     = INDIVIDUAL
getPassType BULKSPONSOR = ORGANIZATION

getCount :: PassApplicationType -> Maybe Int -> L.Flow Int
getCount SELF _ = return 1
getCount SPONSOR _ = return 1
getCount BULKSPONSOR (Just c) = return c
getCount BULKSPONSOR Nothing = L.throwException $ err400 {errBody = "Count cannot be null"}

mapIdType MOBILE  = CD.MOBILENUMBER
mapIdType AADHAAR = CD.AADHAAR

createCustomer :: Text -> L.Flow Customer.Customer
createCustomer name = do
  id <- generateGUID
  Customer.create =<< (getCust id)
  Customer.findCustomerById id
    >>= fromMaybeM500 "Unable to create customer"
  where
    getCust id = do
      now <- getCurrentTimeUTC
      return $ Customer.Customer
        { _id = id
        , _name = Just name
        , _OrganizationId = Nothing
        , _TenantOrganizationId = Nothing
        , _verified = False
        , _role = Customer.INDIVIDUAL
        , _info = Nothing
        , _createdAt = now
        , _updatedAt = now
        }
