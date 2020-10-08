{-# LANGUAGE OverloadedLabels #-}

module Product.Delhivery.Transform where

import App.Types
import Beckn.Types.API.Callback
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Core.Address as CoreAddr
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Context
import Beckn.Types.Core.DecimalValue
import Beckn.Types.Core.Descriptor
import qualified Beckn.Types.Core.Error as CoreErr
import Beckn.Types.Core.Item
import qualified Beckn.Types.Core.Location as CoreLoc
import Beckn.Types.Core.Person
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.Catalog
import Beckn.Types.FMD.Task
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common (fromMaybeM500, getCurrTime, headMaybe, throwError400)
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import EulerHS.Prelude hiding (drop)
import External.Delhivery.Types
import Types.Wrapper
import Utils.Common

mkQuoteReqFromSearch :: SearchReq -> Flow QuoteReq
mkQuoteReqFromSearch SearchReq {..} = do
  let intent = message ^. #intent
      pickups = intent ^. #_pickups
      drops = intent ^. #_drops
  case (pickups, drops) of
    ([pickup], [drop]) -> do
      pickupLoc <- mkLocDetails pickup
      dropLoc <- mkLocDetails drop
      return $
        QuoteReq
          { inv = Nothing,
            itm = [],
            oid = Nothing,
            cod = Nothing,
            src = pickupLoc,
            tar = dropLoc
          }
    ([_], _) -> onePickupLocationExpected
    _ -> oneDropLocationExpected
  where
    onePickupLocationExpected = throwError400 "ONE_PICKUP_LOCATION_EXPECTED"
    oneDropLocationExpected = throwError400 "ONE_DROP_LOCATION_EXPECTED"
    mkLocDetails loc = do
      address <- mkAddress (loc ^. #_location)
      return $
        LocationDetails
          { eml = Nothing,
            pho = "",
            nam = "",
            det = address
          }

mkOnSearchReq :: Organization -> Context -> QuoteRes -> Flow OnSearchReq
mkOnSearchReq _ context res@QuoteRes {..} = do
  now <- getCurrTime
  cid <- generateGUID
  itemId <- generateGUID
  return $
    CallbackReq
      { context = context & #_action .~ "on_search",
        contents = Right $ OnSearchServices (catalog cid now itemId)
      }
  where
    catalog cid now itemId =
      Catalog
        { _id = cid,
          _categories = [],
          _brands = [],
          _models = [],
          _ttl = now,
          _items = [mkSearchItem itemId res],
          _offers = [],
          _package_categories = []
        }

mkItemDetails :: Item -> ItemDetails
mkItemDetails item =
  let prdDesc = item ^. #_descriptor . #_short_desc . _Just
   in ItemDetails
        { prd = prdDesc,
          qty = 1,
          inv = Nothing,
          cod = Nothing
        }

mkLocationDetails :: PickupOrDrop -> Flow LocationDetails
mkLocationDetails PickupOrDrop {..} = do
  phone <- headMaybe (_poc ^. #phones) & fromMaybeM500 "PERSON_PHONENUMBER_NOT_FOUND"
  address <- mkAddress _location
  return $
    LocationDetails
      { eml = _poc ^. #email,
        pho = phone,
        nam = getName (_poc ^. #name),
        det = address
      }
  where
    getName :: Name -> Text
    getName Name {..} =
      let def = maybe "" (" " <>)
       in def _honorific_prefix
            <> def _honorific_suffix
            <> _given_name
            <> def _additional_name
            <> def _family_name

mkAddress :: CoreLoc.Location -> Flow Address
mkAddress location = do
  (CoreLoc.GPS lat lon) <- CoreLoc._gps location & fromMaybeM500 "LAT_LON_NOT_FOUND"
  address <- CoreLoc._address location & fromMaybeM500 "ADDRESS_NOT_FOUND"
  return $
    Address
      { cty = CoreAddr._city address,
        add =
          CoreAddr._door address
            <> " "
            <> fromMaybe "" (CoreAddr._name address)
            <> " "
            <> fromMaybe "" (CoreAddr._building address),
        cnt = Just $ CoreAddr._country address,
        crd = lat <> "," <> lon,
        reg = CoreAddr._state address,
        zip = Just $ CoreAddr._area_code address
      }

mkSearchItem :: Text -> QuoteRes -> Item
mkSearchItem itemId QuoteRes {..} =
  Item
    { _id = itemId,
      _parent_item_id = Nothing,
      _descriptor = emptyDescriptor,
      _price = price,
      _model_id = Nothing,
      _category_id = Nothing,
      _package_category_id = Nothing,
      _brand_id = Nothing,
      _promotional = False,
      _ttl = Nothing,
      _tags = [Tag "eta" (T.pack $ show eta)]
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
    value = convertAmountToDecimalValue (Amount $ toRational pricing)

mkOnSearchErrReq :: Context -> Error -> OnSearchReq
mkOnSearchErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_search",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = "DOMAIN-ERROR",
          _code = "",
          _path = Nothing,
          _message = Just $ err ^. #message
        }

updateBppUri :: Context -> BaseUrl -> Context
updateBppUri Context {..} bpNwAddress = Context {_bpp_uri = Just bpNwAddress, ..}

getDlBAPCreds :: Organization -> Flow DlBAConfig
getDlBAPCreds = getClientConfig
