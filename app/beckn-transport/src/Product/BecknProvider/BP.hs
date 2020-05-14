{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Location as BL
import Beckn.Types.Mobility.Intent
import Beckn.Types.Storage.Case as SC
import Beckn.Types.Storage.CaseProduct as CaseProduct
import Beckn.Types.Storage.Location as SL
import Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Accessor as Lens
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.CaseProduct as CaseProduct
import Storage.Queries.Location as Loc
import Storage.Queries.Organization as Org
import Storage.Queries.Person as Person
import Storage.Queries.Products as Product
import Types.Notification
import Utils.FCM

-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using GCM
-- 3) Respond with Ack

search :: SearchReq -> FlowHandler AckResponse
search req = withFlowHandler $ do
  --TODO: Need to add authenticator
  uuid <- L.generateGUID
  currTime <- getCurrentTimeUTC
  validity <- getValidTime $ req ^. #message ^. #time
  uuid1 <- L.generateGUID
  let fromLocation = mkFromLocation req uuid1 currTime $ req ^. #message ^. #origin
  uuid2 <- L.generateGUID
  let toLocation = mkFromLocation req uuid2 currTime $ req ^. #message ^. #destination
  Loc.create fromLocation
  Loc.create toLocation
  let c = mkCase req uuid currTime validity fromLocation toLocation
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  -- TODO : Fix show
  admins <-
    findAllByOrgIds
      [Person.ADMIN]
      ((\o -> _getOrganizationId $ Org._id o) <$> transporters)
  -- notifyTransporters c admins --TODO : Uncomment this once we start saving deviceToken
  mkAckResponse uuid "search"

notifyTransporters :: Case -> [Person] -> L.Flow ()
notifyTransporters c admins =
  -- TODO : Get Token from Person
  traverse_ (\p -> sendNotification mkCaseNotification "deviceToken") admins
  where
    mkCaseNotification =
      Notification
        { _type = LEAD,
          _payload = c
        }

getValidTime :: LocalTime -> L.Flow LocalTime
getValidTime now = pure $ addLocalTime (60 * 30 :: NominalDiffTime) now

mkFromLocation :: SearchReq -> Text -> LocalTime -> BL.Location -> SL.Location
mkFromLocation req uuid now loc = do
  case loc ^. #_type of
    "gps" -> case loc ^. #_gps of
      Just (val :: GPS) -> mkLocationRecord uuid now SL.POINT (Just $ read $ T.unpack $ val ^. #lat) (Just $ read $ T.unpack $ val ^. #lon) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      _ -> undefined -- need to throw error
    "address" -> case loc ^. #_address of
      Just (val :: Address) -> mkLocationRecord uuid now SL.ADDRESS Nothing Nothing (Just $ val ^. #area) Nothing (Just $ val ^. #area) Nothing (Just $ val ^. #country) (Just $ val ^. #area_code) (Just $ (val ^. #door) <> (val ^. #building) <> (val ^. #street)) Nothing
      _ -> undefined -- need to throw error
    _ -> mkLocationRecord uuid now SL.POINT Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkLocationRecord ::
  Text ->
  LocalTime ->
  SL.LocationType ->
  Maybe Double ->
  Maybe Double ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  SL.Location
mkLocationRecord idr time typ lat lon ward dis city state country pincode address bound =
  SL.Location
    { _id = LocationId {_getLocationId = idr},
      _locationType = typ,
      _lat = lat,
      _long = lon,
      _ward = ward,
      _district = dis,
      _city = city,
      _state = state,
      _country = country,
      _pincode = pincode,
      _address = address,
      _bound = bound,
      _createdAt = time,
      _updatedAt = time
    }

mkCase :: SearchReq -> Text -> LocalTime -> LocalTime -> SL.Location -> SL.Location -> SC.Case
mkCase req uuid now validity fromLocation toLocation = do
  let intent = (req ^. #message)
  SC.Case
    { _id = CaseId {_getCaseId = uuid},
      _name = Nothing,
      _description = Just "Case to create a Ride",
      _shortId = req ^. #context ^. #transaction_id,
      _industry = SC.MOBILITY,
      _type = RIDEBOOK,
      _exchangeType = FULFILLMENT,
      _status = NEW,
      _startTime = intent ^. #time,
      _endTime = Nothing,
      _validTill = validity,
      _provider = Nothing,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Just CONSUMER,
      _parentCaseId = Nothing,
      _fromLocationId = fromLocation ^. #_id ^. #_getLocationId,
      _toLocationId = toLocation ^. #_id ^. #_getLocationId,
      _udf1 = Just $ intent ^. #vehicle ^. #variant,
      _udf2 = Just $ show $ intent ^. #payload ^. #travellers ^. #count,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = Nothing,
      _info = Nothing, --Just $ show $ req ^. #message
      _createdAt = now,
      _updatedAt = now
    }

confirm :: ConfirmReq -> FlowHandler AckResponse
confirm req = withFlowHandler $ do
  L.logInfo "confirm API Flow" "Reached"
  let prodId = (req ^. #message ^. #_selected_items) !! 0
  let caseShortId = req ^. #context ^. #transaction_id -- change to message.transactionId
  case_ <- Case.findBySid caseShortId
  let caseId = _getCaseId $ case_ ^. #_id
  Case.updateStatus (CaseId caseId) SC.CONFIRMED
  CaseProduct.updateStatus (CaseId caseId) (ProductsId prodId) CaseProduct.CONFIRMED
  Product.updateStatus (ProductsId prodId) Product.CONFIRMED
  uuid <- L.generateGUID
  mkAckResponse uuid "confirm"

-- TODO : Add notifying transporter admin with GCM
