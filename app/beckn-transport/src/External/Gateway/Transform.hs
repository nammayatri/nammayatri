{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import Beckn.Types.App
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Category
import Beckn.Types.Core.Context
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Products
import Beckn.Types.Storage.Products as Product
import Data.Aeson
import Data.Map
import Data.Text.Encoding as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant

mkCatalog :: [Products] -> Catalog
mkCatalog prods =
  Catalog
    { _category_tree = Category {_id = "", _subcategories = []},
      _items = mkItem <$> prods
    }

mkItem :: Products -> Item
mkItem prod =
  Item
    { _id = _getProductsId $ prod ^. #_id,
      _description = fromMaybe "" $ prod ^. #_description,
      _name = fromMaybe "" $ prod ^. #_name,
      _image = Nothing,
      _price = mkPrice prod,
      _primary = False,
      _selected = False,
      _quantity = 1,
      _policy = Nothing,
      _category_id = "",
      _tags = []
    }

mkPrice :: Products -> Price
mkPrice prod =
  Price
    { _currency = "INR", -- TODO : Fetch this from product
      _estimated_value = prod ^. #_price,
      _computed_value = prod ^. #_price,
      _listed_value = prod ^. #_price,
      _offered_value = prod ^. #_price,
      _unit = "Rs", -- TODO : Fetch this from product
      _discount = 0.0,
      _tax = Nothing
    }

mkServiceOffer :: Case -> [Products] -> Maybe Trip -> L.Flow Service
mkServiceOffer c prods trip =
  let x =
        Service
          { _id = _getCaseId $ c ^. #_id,
            _catalog = Just $ mkCatalog prods,
            _matched_items = (_getProductsId . Product._id) <$> prods,
            _selected_items = catMaybes $ (\x -> if x ^. #_status == Product.CONFIRMED then Just (_getProductsId $ x ^. #_id) else Nothing) <$> prods,
            _fare_product = Nothing,
            _offers = [],
            _provider = Nothing,
            _trip = trip,
            _policies = [],
            _billing_address = Nothing
          }
   in return x

mkTrip :: Maybe Case -> L.Flow (Maybe Trip)
mkTrip maybeCase = case maybeCase of
  Nothing -> return Nothing
  Just c -> do
    let data_url = baseTrackingUrl <> "/" <> (_getCaseId $ c ^. #_id)
        embed_url = baseTrackingUrl <> "/" <> (_getCaseId $ c ^. #_id) <> "/embed"
    --TODO: get case product and product then use it to fetch details and prepare
    return $
      Just
        Trip
          { id = _getCaseId $ c ^. #_id,
            vehicle = Nothing, -- TODO: need to take it from product
            driver = Nothing, -- TODO: need to take it from product.assginedTo
            travellers = [],
            tracking = mkTracking "PULL" data_url embed_url,
            corridor_type = "ON-DEMAND",
            state = "", -- TODO: need to take it from product
            fare = Nothing, -- TODO: need to take it from product
            route = Nothing
          }

baseTrackingUrl :: Text
baseTrackingUrl = "http://api.sandbox.beckn.juspay.in/transport/v1/location"

mkTracking :: Text -> Text -> Text -> Tracking
mkTracking method dataUrl embedUrl =
  Tracking
    { method = method,
      pull = if method == "PULL" then Just $ mkPullTrackingData dataUrl embedUrl else Nothing
    }

mkPullTrackingData :: Text -> Text -> PullTrackingData
mkPullTrackingData dataUrl embed =
  PullTrackingData
    { data_url = dataUrl,
      embed_url = embed
    }
