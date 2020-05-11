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
import Beckn.Types.Storage.Location as SL
import Beckn.Types.Storage.Organization as Org
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.Products as Product
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Accessor as Lens
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.Location as Loc
import Storage.Queries.Organization as Org
import Storage.Queries.Person as Person
import Storage.Queries.Products as Product
import Types.Notification
import Utils.FCM

-- 1) Create Parent Case with Customer Request Details
-- 2) Notify all transporter using GCM
-- 3) Respond with Ack

search :: Text -> SearchReq -> FlowHandler SearchRes
search apiKey req = withFlowHandler $ do
  uuid <- L.generateGUID
  -- get customer info and do findOrCreate Person?? or store customerInfo in requestor?
  currTime <- getCurrentTimeUTC
  validity <- getValidTime currTime
  let fromLocation = mkFromLocation req uuid "1" currTime $ req ^. #message ^. #origin
  let toLocation = mkFromLocation req uuid "2" currTime $ req ^. #message ^. #destination
  Loc.create fromLocation
  Loc.create toLocation
  let c = mkCase req uuid currTime validity fromLocation toLocation
  Case.create c
  transporters <- listOrganizations Nothing Nothing [Org.TRANSPORTER] [Org.APPROVED]
  -- TODO : Fix show
  admins <-
    findAllByOrgIds
      [Person.ADMIN]
      ((\o -> show $ Org._id o) <$> transporters)
  -- notifyTransporters c admins TODO : Uncomment this once we start saving deviceToken
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

mkFromLocation :: SearchReq -> Text -> Text -> LocalTime -> BL.Location -> SL.Location
mkFromLocation req uuid ref now loc = do
  case loc ^. #_type of
    "gps" -> case loc ^. #_gps of
      Just (val :: GPS) ->
        SL.Location
          { _id = LocationId {_getLocationId = uuid <> ref},
            _locationType = POINT,
            _lat = Just $ val ^. #lat,
            _long = Just $ val ^. #lon,
            _ward = Nothing,
            _district = Nothing,
            _city = Nothing,
            _state = Nothing,
            _country = Nothing,
            _pincode = Nothing,
            _address = Nothing,
            _bound = Nothing,
            _createdAt = now,
            _updatedAt = now
          }
      _ -> undefined -- need to throw error
    _ ->
      SL.Location
        { _id = LocationId {_getLocationId = uuid <> "1"},
          _locationType = POINT,
          _lat = Nothing,
          _long = Nothing,
          _ward = Nothing,
          _district = Nothing,
          _city = Nothing,
          _state = Nothing,
          _country = Nothing,
          _pincode = Nothing,
          _address = Nothing,
          _bound = Nothing,
          _createdAt = now,
          _updatedAt = now
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
      _startTime = now,
      _endTime = Nothing,
      _validTill = validity,
      _provider = Nothing,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Just CONSUMER,
      _parentCaseId = Nothing,
      _fromLocationId = fromLocation ^. #_id,
      _toLocationId = toLocation ^. #_id,
      _udf1 = Just $ intent ^. #vehicle ^. #variant,
      _udf2 = Just $ show $ intent ^. #payload ^. #travellers ^. #count,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = Nothing,
      _info = Just $ show $ req ^. #message,
      _createdAt = now,
      _updatedAt = now
    }

confirm :: Text -> ConfirmReq -> FlowHandler AckResponse
confirm apiKey req = withFlowHandler $ do
  let prodId = (req ^. #message ^. #_selected_items) !! 0
  Product.updateStatus (ProductsId prodId) Product.INPROGRESS
  uuid <- L.generateGUID
  mkAckResponse uuid "confirm"

-- TODO : Add notifying transporter admin with GCM
