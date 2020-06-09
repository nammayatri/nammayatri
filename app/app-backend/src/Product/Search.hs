{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import Beckn.External.FCM.Flow as FCM
import Beckn.External.FCM.Types
import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.Common
  ( AckResponse (..),
    generateGUID,
  )
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Item as Core
import qualified Beckn.Types.Core.Location as Core
import qualified Beckn.Types.Core.Provider as Core
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Products as Products
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Beckn.Utils.Common (encodeToText, fromMaybeM500, withFlowHandler)
import Beckn.Utils.Extra
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.LocalTime (addLocalTime)
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import Servant
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products
import System.Environment
import Types.App
import Types.ProductInfo
import Utils.Common
  ( generateShortId,
    verifyToken,
  )

search :: Maybe RegToken -> SearchReq -> FlowHandler SearchRes
search regToken req = withFlowHandler $ do
  token <- verifyToken regToken
  person <-
    Person.findById (PersonId $ RegistrationToken._EntityId token)
      >>= fromMaybeM500 "Could not find user"
  validateDateTime req
  fromLocation <- mkLocation (req ^. #message ^. #origin)
  toLocation <- mkLocation (req ^. #message ^. #destination)
  Location.create fromLocation
  Location.create toLocation
  case_ <- mkCase req (_getPersonId $ person ^. #_id) fromLocation toLocation
  Case.create case_
  gatewayUrl <- Gateway.getBaseUrl
  eres <- Gateway.search gatewayUrl $ req & (#context . #transaction_id) .~ (_getCaseId $ case_ ^. #_id)
  let ack =
        case eres of
          Left err -> Ack "Error" (show err)
          Right _ -> Ack "Successful" (_getCaseId $ case_ ^. #_id)
  return $ AckResponse (req ^. #context) ack
  where
    validateDateTime req = do
      currTime <- getCurrentTimeUTC
      when ((req ^. #message ^. #time) < currTime)
        $ L.throwException
        $ err400 {errBody = "Invalid start time"}

search_cb :: Maybe RegToken -> OnSearchReq -> FlowHandler OnSearchRes
search_cb regToken req = withFlowHandler $ do
  -- TODO: Verify api key here
  let service = req ^. #message
      mprovider = service ^. #_provider
      mcatalog = service ^. #_catalog
      caseId = CaseId $ req ^. #context ^. #transaction_id --CaseId $ service ^. #_id
  case mcatalog of
    Nothing -> return ()
    Just catalog -> do
      case_ <- Case.findById caseId
      when
        (case_ ^. #_status == Case.CLOSED)
        (L.throwException $ err400 {errBody = "Case expired"})
      personId <-
        maybe
          (L.throwException $ err500 {errBody = "No person linked to case"})
          (return . PersonId)
          (Case._requestor case_)
      let items = catalog ^. #_items
      products <- traverse (mkProduct case_ mprovider) items
      let pids = (^. #_id) <$> products
      traverse_ Products.create products
      traverse_
        (\product -> mkCaseProduct caseId personId product >>= CaseProduct.create)
        products
      notifyOnSearchCb personId caseId
  let ack = Ack "on_search" "OK"
  return $ AckResponse (req ^. #context) ack

mkCase :: SearchReq -> Text -> Location.Location -> Location.Location -> L.Flow Case.Case
mkCase req userId from to = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  -- TODO: consider collision probability for shortId
  -- Currently it's a random 10 char alphanumeric string
  -- If the insert fails, maybe retry automatically as there
  -- is a unique constraint on `shortId`
  shortId <- generateShortId
  let intent = req ^. #message
      context = req ^. #context
  validTill <- getCaseExpiry (req ^. #message ^. #time)
  return $
    Case.Case
      { _id = id,
        _name = Nothing,
        _description = Just "Case to create a Ride",
        _shortId = shortId,
        _industry = Case.MOBILITY,
        _type = Case.RIDEBOOK,
        _exchangeType = Case.FULFILLMENT,
        _status = Case.NEW,
        _startTime = req ^. #message ^. #time,
        _endTime = Nothing,
        _validTill = validTill,
        _provider = Nothing,
        _providerType = Nothing,
        _requestor = Just userId,
        _requestorType = Just Case.CONSUMER,
        _parentCaseId = Nothing,
        _fromLocationId = from ^. #_id ^. #_getLocationId,
        _toLocationId = to ^. #_id ^. #_getLocationId,
        _udf1 = Just $ intent ^. #vehicle ^. #variant,
        _udf2 = Just $ show $ intent ^. #payload ^. #travellers ^. #count,
        _udf3 = Nothing,
        _udf4 = Just $ context ^. #transaction_id,
        _udf5 = Nothing,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now
      }
  where
    getCaseExpiry :: LocalTime -> L.Flow LocalTime
    getCaseExpiry startTime = do
      now <- getCurrentTimeUTC
      caseExpiry <- pure . fromMaybe 7200 . join . (readMaybe <$>) =<< L.runIO (lookupEnv "DEFAULT_CASE_EXPIRY")
      let minExpiry = 300 -- 5 minutes
          timeToRide = startTime `diffLocalTime` now
          defaultExpiry = (fromInteger caseExpiry) `addLocalTime` now
          validTill = addLocalTime (minimum [(fromInteger caseExpiry), maximum [minExpiry, timeToRide]]) now
      pure validTill

mkLocation :: Core.Location -> L.Flow Location.Location
mkLocation loc = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  let mgps = loc ^. #_gps
  return $
    Location.Location
      { _id = id,
        _locationType = Location.POINT,
        _lat = (read . T.unpack . (^. #lat)) <$> mgps,
        _long = (read . T.unpack . (^. #lon)) <$> mgps,
        _ward = Nothing,
        _district = Nothing,
        _city = (^. #name) <$> loc ^. #_city,
        _state = Nothing,
        _country = (^. #name) <$> loc ^. #_country,
        _pincode = Nothing,
        _address = (T.decodeUtf8 . BSL.toStrict . encode) <$> loc ^. #_address,
        _bound = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

mkProduct :: Case.Case -> Maybe Core.Provider -> Core.Item -> L.Flow Products.Products
mkProduct case_ mprovider item = do
  now <- getCurrentTimeUTC
  let validTill = addLocalTime (60 * 30) now
  let info = ProductInfo mprovider Nothing
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transporter and app-backend
  -- TODO: fit public transport, where case.startTime != product.startTime, etc
  return $
    Products.Products
      { _id = ProductsId $ item ^. #_id,
        _shortId = "",
        _name = Just $ item ^. #_name,
        _description = Just $ item ^. #_description,
        _industry = case_ ^. #_industry,
        _type = Products.RIDE,
        _status = Products.INSTOCK,
        _startTime = case_ ^. #_startTime,
        _endTime = Nothing, -- TODO: fix this
        _validTill = case_ ^. #_validTill,
        _price = item ^. #_price ^. #_listed_value,
        _rating = Nothing,
        _review = Nothing,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = Nothing,
        _fromLocation = Just $ case_ ^. #_fromLocationId,
        _toLocation = Just $ case_ ^. #_toLocationId,
        _info = Just $ encodeToText info,
        _organizationId = "", -- TODO: fix this
        _assignedTo = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

mkCaseProduct :: CaseId -> PersonId -> Products.Products -> L.Flow CaseProduct.CaseProduct
mkCaseProduct caseId personId product = do
  let productId = product ^. #_id
      price = product ^. #_price
  now <- getCurrentTimeUTC
  id <- generateGUID
  return $
    CaseProduct.CaseProduct
      { _id = id,
        _caseId = caseId,
        _productId = productId,
        _personId = Just personId,
        _quantity = 1,
        _price = price,
        _status = Products.INSTOCK,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

notifyOnSearchCb :: PersonId -> CaseId -> L.Flow ()
notifyOnSearchCb personId caseId = do
  person <- Person.findById personId
  case person of
    Just p -> do
      let notificationData =
            FCMData
              { _fcmNotificationType = "SEARCH_CALLBACK",
                _fcmShowNotification = "true",
                _fcmEntityIds = show $ _getCaseId caseId,
                _fcmEntityType = "Case"
              }
          title = "New ride options available!"
          body = T.pack "You have a new reply for your ride request! Head to the beckn app for details."
      FCM.notifyPerson title body notificationData p
      pure ()
    _ -> pure ()
