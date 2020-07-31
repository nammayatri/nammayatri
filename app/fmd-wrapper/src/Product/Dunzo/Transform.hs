{-# LANGUAGE OverloadedLabels #-}

module Product.Dunzo.Transform where

import App.Types
import Beckn.Types.Common (generateGUID)
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.FMD.API.Search
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (throwJsonError400, throwJsonError500)
import Beckn.Utils.Extra (getCurrentTimeUTC)
import qualified Data.Text as T
import EulerHS.Prelude
import External.Dunzo.Types
import Types.Wrapper
import Utils.Common (getClientConfig)

getDunzoConfig :: Organization -> Flow (ClientId, ClientSecret, Text, [BAConfig])
getDunzoConfig org = do
  config <- getClientConfig org
  case config of
    Dunzo dzClientId dzClientSecret dzUrl dzBAConfigs -> return (dzClientId, dzClientSecret, dzUrl, dzBAConfigs)
    _ -> throwJsonError500 "CLIENT_CONFIG" "MISMATCH"

mkQuoteReq :: SearchReq -> Flow QuoteReq
mkQuoteReq SearchReq {..} = do
  let intent = message ^. #intent
      pickups = intent ^. #_pickups
      drops = intent ^. #_drops
      packages = intent ^. #_packages
      items = concat $ (\pkg -> pkg ^. #_contents) <$> packages
  when
    (length pickups /= 1)
    (throwJsonError400 "NUMBER_OF_PICKUPS" "EXCEEDES_LIMIT")
  when
    (length drops /= 1)
    (throwJsonError400 "NUMBER_OF_DROPS" "EXCEEDES_LIMIT")
  when
    (null items)
    (throwJsonError400 "NUMBER_OF_ITEMS" "NONE")
  let pickup1 = head pickups
      drop1 = head drops
      pgps = pickup1 ^. #_gps
      dgps = drop1 ^. #_gps
  when
    (isNothing pgps)
    (throwJsonError400 "PICKUP_LOCATION" "NOT_FOUND")
  when
    (isNothing dgps)
    (throwJsonError400 "DROP_LOCATION" "NOT_FOUND")
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
        category_id = (\item -> item ^. #_category_id) $ head items
      }

readCoord :: Text -> Flow Double
readCoord text = do
  let mCoord = readMaybe $ T.unpack text
  case mCoord of
    Nothing -> throwJsonError400 "LOCATION" "READ_ERROR"
    Just v -> return v

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
      _ttl = Just $ eta ^. #pickup + eta ^. #drop, -- FIX this
      _tags = []
    }
  where
    price =
      Price
        { _currency = "INR",
          _value = value,
          _estimated_value = value,
          _computed_value = value,
          _listed_value = value,
          _offered_value = value,
          _minimum_value = value,
          _maximum_value = value
        }
    value = DecimalValue "" "" -- FIX this
    descriptor = Descriptor n n n n n [] n n
    n = Nothing
