{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import Beckn.Types.App
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Category
import Beckn.Types.Core.Context
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Service
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

mkServiceOffer :: Case -> [Products] -> [Text] -> L.Flow Service
mkServiceOffer c prods prodId =
  let x =
        Service
          { _id = _getCaseId $ c ^. #_id,
            _catalog = Just $ mkCatalog prods,
            _matched_items = (_getProductsId . Product._id) <$> prods,
            _selected_items = prodId,
            _fare_product = Nothing,
            _offers = [],
            _provider = Nothing,
            _trip = Nothing,
            _policies = [],
            _billing_address = Nothing
          }
   in return x
