{-# LANGUAGE OverloadedLabels #-}

module Product.Search where

import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Item as Core
import qualified Beckn.Types.Core.Location as Core
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseProduct
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Products as Products
import Beckn.Utils.Common (getCurrTime, withFlowHandler)
import qualified Data.Text as T
import Data.Time.LocalTime (addLocalTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Location as Location
import qualified Storage.Queries.Products as Products
import Types.App

search :: Maybe RegToken -> SearchReq -> FlowHandler SearchRes
search regToken req = withFlowHandler $ do
  fromLocation <- mkLocation (req ^. #message ^. #origin)
  toLocation <- mkLocation (req ^. #message ^. #destination)
  Location.create fromLocation
  Location.create toLocation
  case_ <- mkCase req fromLocation toLocation
  Case.create case_

  gatewayUrl <- Gateway.getBaseUrl
  eres <- Gateway.search gatewayUrl req
  let ack =
        case eres of
          Left err -> Ack "search" ("Err: " <> show err)
          Right _ -> Ack "search" (show $ case_ ^. #_id)
  return $ AckResponse (req ^. #context) ack

search_cb :: Maybe RegToken -> OnSearchReq -> FlowHandler OnSearchRes
search_cb regToken req = withFlowHandler $ do
  let service = req ^. #message
      mcatalog = service ^. #_catalog
      caseId = CaseId $ service ^. #_id

  case mcatalog of
    Nothing -> return ()
    Just catalog -> do
      let items = catalog ^. #_items
      products <- traverse mkProduct items
      let pids = (^. #_id) <$> products
      traverse_ Products.create products

      traverse_
        (\product -> mkCaseProduct caseId product >>= CaseProduct.create)
        products

  let ack = Ack "on_search" "OK"
  return $ AckResponse (req ^. #context) ack

mkCase :: SearchReq -> Location.Location -> Location.Location -> L.Flow Case.Case
mkCase req from to = do
  now <- getCurrTime
  id <- generateGUID
  let intent = req ^. #message
      context = req ^. #context
      validTill = addLocalTime (60 * 30) now
  return $
    Case.Case
      { _id = id,
        _name = Nothing,
        _description = Just "Case to create a Ride",
        _shortId = context ^. #transaction_id,
        _industry = Case.MOBILITY,
        _type = Case.RIDEBOOK,
        _exchangeType = Case.FULFILLMENT,
        _status = Case.NEW,
        _startTime = now,
        _endTime = Nothing,
        _validTill = validTill,
        _provider = Nothing,
        _providerType = Nothing,
        _requestor = Nothing,
        _requestorType = Just Case.CONSUMER,
        _parentCaseId = Nothing,
        _fromLocationId = from ^. #_id ^. #_getLocationId,
        _toLocationId = to ^. #_id ^. #_getLocationId,
        _udf1 = Just $ intent ^. #vehicle ^. #variant,
        _udf2 = Just $ show $ intent ^. #payload ^. #travellers ^. #count,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = Nothing,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

mkLocation :: Core.Location -> L.Flow Location.Location
mkLocation loc = do
  now <- getCurrTime
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
        _address = show <$> loc ^. #_address,
        _bound = Nothing,
        _createdAt = now,
        _updatedAt = now
      }

mkProduct :: Core.Item -> L.Flow Products.Products
mkProduct item = do
  now <- getCurrTime
  let validTill = addLocalTime (60 * 30) now
  -- There is loss of data in coversion Product -> Item -> Product
  -- In api exchange between transpoter and app-backend
  return $
    Products.Products
      { _id = ProductsId $ item ^. #_id,
        _name = Just $ item ^. #_name,
        _description = Just $ item ^. #_description,
        _industry = Products.MOBILITY, -- TODO: fix this
        _type = Products.RIDE,
        _status = Products.INSTOCK,
        _startTime = now, -- TODO: fix this
        _endTime = Nothing, -- TODO: fix this
        _validTill = validTill,
        _price = item ^. #_price ^. #_listed_value,
        _rating = Nothing,
        _review = Nothing,
        _udf1 = Nothing,
        _udf2 = Nothing,
        _udf3 = Nothing,
        _udf4 = Nothing,
        _udf5 = Nothing,
        _info = Nothing,
        _fromLocation = Nothing, -- TODO: fix this
        _toLocation = Nothing, -- TODO: fix this
        _organizationId = "", -- TODO: fix this
        _createdAt = now,
        _updatedAt = now
      }

mkCaseProduct :: CaseId -> Products.Products -> L.Flow CaseProduct.CaseProduct
mkCaseProduct caseId product = do
  let productId = product ^. #_id
      price = product ^. #_price
  now <- getCurrTime
  id <- generateGUID
  return $
    CaseProduct.CaseProduct
      { _id = id,
        _caseId = caseId,
        _productId = productId,
        _quantity = 1,
        _price = price,
        _status = CaseProduct.INSTOCK,
        _info = Nothing,
        _createdAt = now,
        _updatedAt = now
      }
