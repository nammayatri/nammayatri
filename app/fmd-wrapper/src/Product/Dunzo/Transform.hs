{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.Common (generateGUID)
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Descriptor
import qualified Beckn.Types.Core.Error as Err
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.Core.Quotation
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (throwJsonError400, throwJsonError500)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import qualified Data.Text as T
import EulerHS.Prelude
import External.Dunzo.Types
import Types.Wrapper
import Utils.Common (getClientConfig)

data DunzoConfig = DunzoConfig
  { clientId :: ClientId,
    clientSecret :: ClientSecret,
    url :: Text,
    baConfigs :: [BAConfig],
    bpId :: Text,
    bpNwAddr :: Text
  }
  deriving (Show)

getDunzoConfig :: Organization -> Flow DunzoConfig
getDunzoConfig org = do
  config <- getClientConfig org
  case config of
    Dunzo dzClientId dzClientSecret dzUrl dzBAConfigs dzBPId dzBPNwAddress ->
      return $ DunzoConfig dzClientId dzClientSecret dzUrl dzBAConfigs dzBPId dzBPNwAddress
    _ -> throwJsonError500 "CLIENT_CONFIG" "MISMATCH"

mkQuoteReq :: SearchReq -> Flow QuoteReq
mkQuoteReq SearchReq {..} = do
  let intent = message ^. #intent
      pickups = intent ^. #_pickups
      drops = intent ^. #_drops
  case (pickups, drops) of
    ([pickup], [drop]) ->
      case (pickup ^. #_location . #_gps, drop ^. #_location . #_gps) of
        (Just pgps, Just dgps) -> do
          plat <- readCoord (pgps ^. #lat)
          plon <- readCoord (pgps ^. #lon)
          dlat <- readCoord (dgps ^. #lat)
          dlon <- readCoord (dgps ^. #lon)
          return $
            QuoteReq
              { pickup_lat = plat,
                pickup_lng = plon,
                drop_lat = dlat,
                drop_lng = dlon,
                category_id = "pickup_drop"
              }
        (Just pgps, Nothing) -> dropLocationNotFound
        _ -> pickupLocationNotFound
    ([pickup], _) -> oneDropLocationExpected
    _ -> onePickupLocationExpected
  where
    onePickupLocationExpected = throwJsonError400 "ERR" "ONE_PICKUP_LOCATION_EXPECTED"
    oneDropLocationExpected = throwJsonError400 "ERR" "ONE_DROP_LOCATION_EXPECTED"
    pickupLocationNotFound = throwJsonError400 "ERR" "PICKUP_LOCATION_NOT_FOUND"
    dropLocationNotFound = throwJsonError400 "ERR" "DROP_LOCATION_NOT_FOUND"

mkNewQuoteReq :: SelectReq -> Flow QuoteReq
mkNewQuoteReq SelectReq {..} = do
  let tasks = message ^. (#order . #_tasks)
      task = head tasks
      pickup = task ^. (#_pickup . #_location)
      drop = task ^. (#_drop . #_location)
      package = task ^. #_package
      items = package ^. #_contents
      pgps = pickup ^. #_gps
      dgps = drop ^. #_gps
  plat <- readCoord (fromJust pgps ^. #lat)
  plon <- readCoord (fromJust pgps ^. #lon)
  dlat <- readCoord (fromJust dgps ^. #lat)
  dlon <- readCoord (fromJust dgps ^. #lon)
  return $
    QuoteReq
      { pickup_lat = plat,
        pickup_lng = plon,
        drop_lat = dlat,
        drop_lng = dlon,
        category_id = "pickup_drop"
      }

readCoord :: Text -> Flow Double
readCoord text = do
  let mCoord = readMaybe $ T.unpack text
  maybe (throwJsonError400 "ERR" "LOCATION_READ_ERROR") pure mCoord

mkOnSearchErrReq :: Organization -> Context -> Error -> Flow OnSearchReq
mkOnSearchErrReq org context res@Error {..} = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  return $
    OnSearchReq
      { context = context,
        message = OnSearchServices (catalog id now),
        error = Just error
      }
  where
    catalog id now =
      Catalog
        { _id = id,
          _categories = [],
          _brands = [],
          _models = [],
          _ttl = now,
          _items = [],
          _offers = []
        }

    error =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }

mkOnSearchReq :: Organization -> Context -> QuoteRes -> Flow OnSearchReq
mkOnSearchReq org context res@QuoteRes {..} = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  itemid <- generateGUID
  return $
    OnSearchReq
      { context = context,
        message = OnSearchServices (catalog id itemid now),
        error = Nothing
      }
  where
    catalog id itemid now =
      Catalog
        { _id = id,
          _categories = [],
          _brands = [],
          _models = [],
          _ttl = now,
          _items = [mkSearchItem itemid res],
          _offers = []
        }

updateContext :: Context -> Text -> Text -> Context
updateContext Context {..} bpId bpNwAddress = Context {_bpp_nw_address = Just bpNwAddress, _bpp_id = Just bpId, ..}

mkSearchItem :: Text -> QuoteRes -> Item
mkSearchItem id QuoteRes {..} =
  Item
    { _id = id,
      _parent_item_id = Nothing,
      _descriptor = descriptor,
      _price = price,
      _model_id = Nothing,
      _category_id = Just category_id,
      _brand_id = Nothing,
      _promotional = False,
      _ttl = Just $ eta ^. #pickup + eta ^. #dropoff, -- FIX this
      _tags = []
    }
  where
    price =
      Price
        { _currency = "INR",
          _value = Nothing,
          _estimated_value = Just value,
          _computed_value = Nothing,
          _listed_value = Nothing,
          _offered_value = Nothing,
          _minimum_value = Nothing,
          _maximum_value = Nothing
        }
    value = convertAmountToDecimalValue (Amount $ toRational estimated_price)
    descriptor = Descriptor n n n n n [] n n
    n = Nothing

mkOnSelectReq :: SelectReq -> QuoteRes -> Flow OnSelectReq
mkOnSelectReq req@SelectReq {..} res@QuoteRes {..} = do
  now <- getCurrentTimeUTC
  id <- generateGUID
  let order = req ^. (#message . #order)
      price = mkPrice estimated_price
      quotation = Quotation {_id = id, _price = price, _ttl = Nothing}
  return $
    OnSelectReq
      { context = context,
        message = OnSelectMessage order (Just quotation),
        error = Nothing
      }
  where
    mkPrice estimatedPrice =
      Price
        { _currency = "INR",
          _value = Nothing,
          _estimated_value = Just $ convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          _computed_value = Nothing,
          _listed_value = Nothing,
          _offered_value = Nothing,
          _minimum_value = Nothing,
          _maximum_value = Nothing
        }

mkOnSelectErrReq :: SelectReq -> Error -> Flow OnSelectReq
mkOnSelectErrReq req res@Error {..} = do
  let context = req ^. #context
  let order = req ^. (#message . #order)
  return $
    OnSelectReq
      { context = context,
        message = OnSelectMessage order Nothing,
        error = Just mkError
      }
  where
    mkError =
      Err.Error
        { _type = "DOMAIN-ERROR",
          _code = code,
          _path = Nothing,
          _message = Just message
        }
