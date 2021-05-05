{-# LANGUAGE OverloadedLabels #-}

module Product.Delhivery.Transform where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.FMD.API.Confirm
import Beckn.Types.FMD.API.Init
import Beckn.Types.FMD.API.Search
import Beckn.Types.FMD.API.Select
import Beckn.Types.FMD.Catalog
import qualified Beckn.Types.FMD.Item as FMD
import Beckn.Types.FMD.Order
import Beckn.Types.FMD.Task
import Beckn.Types.Storage.Organization (Organization)
import Control.Lens ((?~))
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (drop)
import External.Delhivery.Types
import Types.Beckn.API.Callback
import qualified Types.Beckn.Address as CoreAddr
import Types.Beckn.Context
import Types.Beckn.DecimalValue
import Types.Beckn.Descriptor
import qualified Types.Beckn.Error as CoreErr
import qualified Types.Beckn.Item as Core
import qualified Types.Beckn.Location as CoreLoc
import Types.Beckn.MonetaryValue
import Types.Beckn.Payment
import Types.Beckn.PaymentEndpoint
import Types.Beckn.Person
import Types.Beckn.Price
import Types.Beckn.Quotation
import Types.Beckn.Tag
import Types.Error
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
    onePickupLocationExpected = throwError $ InvalidRequest "One pickup location expected."
    oneDropLocationExpected = throwError $ InvalidRequest "One drop location expected"
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
  now <- getCurrentTime
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

mkOnSearchErrReq :: Context -> Error -> OnSearchReq
mkOnSearchErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_search",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          _code = "",
          _path = Nothing,
          _message = Just $ err ^. #message
        }

mkQuoteReqFromSelect :: SelectReq -> Flow QuoteReq
mkQuoteReqFromSelect SelectReq {..} = do
  let order = message ^. #order
      task = head $ order ^. #_tasks
  pickupDet <- mkLocationDetails (task ^. #_pickup)
  dropDet <- mkLocationDetails (task ^. #_drop)
  return $
    QuoteReq
      { inv = Nothing,
        itm = mkItemDetails <$> (order ^. #_items),
        oid = order ^. #_id,
        cod = Nothing,
        src = pickupDet,
        tar = dropDet
      }

mkOnSelectOrder :: Order -> QuoteRes -> Flow SelectOrder
mkOnSelectOrder order res@QuoteRes {..} = do
  quote <- mkQuote res
  task <- updateTaskEta (head $ order ^. #_tasks) eta
  let order' =
        order & #_tasks .~ [task]
          & #_quotation ?~ quote
  return $ SelectOrder order'

mkOnSelectReq :: Context -> SelectOrder -> OnSelectReq
mkOnSelectReq context msg =
  CallbackReq
    { context = context & #_action .~ "on_select",
      contents = Right msg
    }

updateTaskEta :: Task -> Integer -> Flow Task
updateTaskEta task eta = do
  now <- getCurrentTime
  let pickup = task ^. #_pickup
  let pickupEta = addUTCTime (fromInteger eta) now
  let pickup' = pickup & #_time ?~ pickupEta
  return $
    task & #_pickup .~ pickup'

mkOnSelectErrReq :: Context -> Error -> OnSelectReq
mkOnSelectErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_search",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          _code = "",
          _path = Nothing,
          _message = Just $ err ^. #message
        }

mkOnInitMessage :: Text -> Order -> PaymentEndpoint -> InitReq -> QuoteRes -> Flow InitOrder
mkOnInitMessage orderId order payee req QuoteRes {..} = do
  task <- updateTaskEta (head $ order ^. #_tasks) eta
  return $
    InitOrder $
      order & #_id ?~ orderId
        & #_payment ?~ mkPayment payee pricing
        & #_billing .~ billing
        & #_tasks .~ [task]
  where
    billing = req ^. #message . #order . #_billing

mkOnInitReq :: Context -> InitOrder -> OnInitReq
mkOnInitReq context msg =
  CallbackReq
    { context = context & #_action .~ "on_init",
      contents = Right msg
    }

mkOnInitErrReq :: Context -> Error -> OnInitReq
mkOnInitErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_init",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          _code = "",
          _path = Nothing,
          _message = Just $ err ^. #message
        }

mkCreateOrderReq :: Order -> Flow CreateOrderReq
mkCreateOrderReq order = do
  let task = head $ order ^. #_tasks
  pickupDet <- mkLocationDetails (task ^. #_pickup)
  dropDet <- mkLocationDetails (task ^. #_drop)
  returnDet <- mkLocationDetails (task ^. #_return)
  return $
    CreateOrderReq
      { inv = Nothing,
        itm = mkItemDetails <$> (order ^. #_items),
        oid = order ^. #_id,
        cod = Nothing,
        src = pickupDet,
        ret = returnDet,
        tar = dropDet
      }

mkOnConfirmReq :: Context -> Order -> Flow OnConfirmReq
mkOnConfirmReq context order =
  return $
    CallbackReq
      { context = context & #_action .~ "on_confirm",
        contents = Right $ ConfirmResMessage order
      }

mkOnConfirmErrReq :: Context -> Error -> OnConfirmReq
mkOnConfirmErrReq context err =
  CallbackReq
    { context = context & #_action .~ "on_confirm",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          _code = "",
          _path = Nothing,
          _message = Just $ err ^. #message
        }

mkItemDetails :: FMD.Item -> ItemDetails
mkItemDetails item =
  let prdDesc = fromMaybe "" (item ^? #_descriptor . _Just . #_short_desc . _Just)
   in ItemDetails
        { prd = prdDesc,
          qty = 1,
          inv = Nothing,
          cod = Nothing
        }

mkLocationDetails :: PickupOrDrop -> Flow LocationDetails
mkLocationDetails PickupOrDrop {..} = do
  phone <- headMaybe (_poc ^. #phones) & fromMaybeM (InternalError "Person phone number is not present.")
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
  (CoreLoc.GPS lat lon) <- CoreLoc._gps location & fromMaybeM (InternalError "Lat/long not found.")
  address <- CoreLoc._address location & fromMaybeM (InternalError "Address not found.")
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

mkSearchItem :: Text -> QuoteRes -> Core.Item
mkSearchItem itemId QuoteRes {..} =
  Core.Item
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

mkQuote :: QuoteRes -> Flow Quotation
mkQuote QuoteRes {..} = do
  qid <- generateGUID
  return $ Quotation {_id = qid, _price = Just price, _ttl = Nothing, _breakup = Nothing}
  where
    price = mkPrice pricing
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

mkPayment :: PaymentEndpoint -> Float -> Payment
mkPayment payee estimated_price =
  Payment
    { _transaction_id = Nothing,
      _type = Just "PRE-FULFILLMENT",
      _payer = Nothing,
      _payee = Just payee,
      _methods = ["RTGS"],
      _amount = price,
      _state = Nothing,
      _due_date = Nothing,
      _duration = Nothing
    }
  where
    price =
      MonetaryValue
        { _currency = "INR",
          _value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }

updateBppUri :: Context -> BaseUrl -> Context
updateBppUri Context {..} bpNwAddress = Context {_bpp_uri = Just bpNwAddress, ..}

getDlBAPCreds :: Organization -> Flow DlBAConfig
getDlBAPCreds = getClientConfig
