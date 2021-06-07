{-# LANGUAGE OverloadedLabels #-}

module Product.Delhivery.Transform where

import App.Types
import Beckn.Types.Amount
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Storage.Organization (Organization)
import Control.Lens ((?~))
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import EulerHS.Prelude hiding (drop)
import ExternalAPI.Delhivery.Types
import Types.Beckn.API.Callback
import Types.Beckn.API.Confirm
import Types.Beckn.API.Init
import Types.Beckn.API.Search
import Types.Beckn.API.Select
import qualified Types.Beckn.Address as CoreAddr
import Types.Beckn.Catalog
import Types.Beckn.Context
import Types.Beckn.DecimalValue
import Types.Beckn.Descriptor
import qualified Types.Beckn.Error as CoreErr
import qualified Types.Beckn.FmdItem as FMD
import Types.Beckn.FmdOrder
import qualified Types.Beckn.Item as Core
import qualified Types.Beckn.Location as CoreLoc
import Types.Beckn.MonetaryValue
import Types.Beckn.Payment
import Types.Beckn.PaymentEndpoint
import Types.Beckn.Person
import Types.Beckn.Price
import Types.Beckn.Quotation
import Types.Beckn.Tag
import Types.Beckn.Task
import Types.Error
import Types.Wrapper
import Utils.Common

mkQuoteReqFromSearch :: SearchReq -> Flow QuoteReq
mkQuoteReqFromSearch SearchReq {..} = do
  let intent = message.intent
      pickups = intent.pickups
      drops = intent.drops
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
      address <- mkAddress (loc.location)
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
      { context = context & #action .~ "on_search",
        contents = Right $ OnSearchServices (catalog cid now itemId)
      }
  where
    catalog cid now itemId =
      Catalog
        { id = cid,
          categories = [],
          brands = [],
          models = [],
          ttl = now,
          items = [mkSearchItem itemId res],
          offers = [],
          package_categories = []
        }

mkOnSearchErrReq :: Context -> Error -> OnSearchReq
mkOnSearchErrReq context err =
  CallbackReq
    { context = context & #action .~ "on_search",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          code = "",
          path = Nothing,
          message = Just $ err.message
        }

mkQuoteReqFromSelect :: SelectReq -> Flow QuoteReq
mkQuoteReqFromSelect SelectReq {..} = do
  let order = message.order
      task = head $ order.tasks
  pickupDet <- mkLocationDetails (task.pickup)
  dropDet <- mkLocationDetails (task.drop)
  return $
    QuoteReq
      { inv = Nothing,
        itm = mkItemDetails <$> (order.items),
        oid = order.id,
        cod = Nothing,
        src = pickupDet,
        tar = dropDet
      }

mkOnSelectOrder :: Order -> QuoteRes -> Flow SelectOrder
mkOnSelectOrder order res@QuoteRes {..} = do
  quote <- mkQuote res
  task <- updateTaskEta (head $ order.tasks) eta
  let order' =
        order & #tasks .~ [task]
          & #quotation ?~ quote
  return $ SelectOrder order'

mkOnSelectReq :: Context -> SelectOrder -> OnSelectReq
mkOnSelectReq context msg =
  CallbackReq
    { context = context & #action .~ "on_select",
      contents = Right msg
    }

updateTaskEta :: Task -> Integer -> Flow Task
updateTaskEta task eta = do
  now <- getCurrentTime
  let pickup = task.pickup
  let pickupEta = addUTCTime (fromInteger eta) now
  let pickup' = pickup & #time ?~ pickupEta
  return $
    task & #pickup .~ pickup'

mkOnSelectErrReq :: Context -> Error -> OnSelectReq
mkOnSelectErrReq context err =
  CallbackReq
    { context = context & #action .~ "on_search",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          code = "",
          path = Nothing,
          message = Just $ err.message
        }

mkOnInitMessage :: Text -> Order -> PaymentEndpoint -> InitReq -> QuoteRes -> Flow InitOrder
mkOnInitMessage orderId order payee req QuoteRes {..} = do
  task <- updateTaskEta (head $ order.tasks) eta
  return $
    InitOrder $
      order & #id ?~ orderId
        & #payment ?~ mkPayment payee pricing
        & #billing .~ billing
        & #tasks .~ [task]
  where
    billing = req.message.order.billing

mkOnInitReq :: Context -> InitOrder -> OnInitReq
mkOnInitReq context msg =
  CallbackReq
    { context = context & #action .~ "on_init",
      contents = Right msg
    }

mkOnInitErrReq :: Context -> Error -> OnInitReq
mkOnInitErrReq context err =
  CallbackReq
    { context = context & #action .~ "on_init",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          code = "",
          path = Nothing,
          message = Just $ err.message
        }

mkCreateOrderReq :: Order -> Flow CreateOrderReq
mkCreateOrderReq order = do
  let task = head $ order.tasks
  pickupDet <- mkLocationDetails (task.pickup)
  dropDet <- mkLocationDetails (task.drop)
  returnDet <- mkLocationDetails (task._return)
  return $
    CreateOrderReq
      { inv = Nothing,
        itm = mkItemDetails <$> (order.items),
        oid = order.id,
        cod = Nothing,
        src = pickupDet,
        ret = returnDet,
        tar = dropDet
      }

mkOnConfirmReq :: Context -> Order -> Flow OnConfirmReq
mkOnConfirmReq context order =
  return $
    CallbackReq
      { context = context & #action .~ "on_confirm",
        contents = Right $ ConfirmResMessage order
      }

mkOnConfirmErrReq :: Context -> Error -> OnConfirmReq
mkOnConfirmErrReq context err =
  CallbackReq
    { context = context & #action .~ "on_confirm",
      contents = Left errResp
    }
  where
    errResp =
      CoreErr.Error
        { _type = CoreErr.DOMAIN_ERROR,
          code = "",
          path = Nothing,
          message = Just $ err.message
        }

mkItemDetails :: FMD.Item -> ItemDetails
mkItemDetails item =
  let prdDesc = fromMaybe "" (item ^? #descriptor . _Just . #short_desc . _Just)
   in ItemDetails
        { prd = prdDesc,
          qty = 1,
          inv = Nothing,
          cod = Nothing
        }

mkLocationDetails :: PickupOrDrop -> Flow LocationDetails
mkLocationDetails PickupOrDrop {..} = do
  phone <- listToMaybe (poc.phones) & fromMaybeM (InternalError "Person phone number is not present.")
  address <- mkAddress location
  return $
    LocationDetails
      { eml = poc.email,
        pho = phone,
        nam = getName (poc.name),
        det = address
      }
  where
    getName :: Name -> Text
    getName Name {..} =
      let def = maybe "" (" " <>)
       in def honorific_prefix
            <> def honorific_suffix
            <> given_name
            <> def additional_name
            <> def family_name

mkAddress :: CoreLoc.Location -> Flow Address
mkAddress location = do
  (CoreLoc.GPS lat lon) <- CoreLoc.gps location & fromMaybeM (InternalError "Lat/long not found.")
  address <- CoreLoc.address location & fromMaybeM (InternalError "Address not found.")
  return $
    Address
      { cty = CoreAddr.city address,
        add =
          CoreAddr.door address
            <> " "
            <> fromMaybe "" (CoreAddr.name address)
            <> " "
            <> fromMaybe "" (CoreAddr.building address),
        cnt = Just $ CoreAddr.country address,
        crd = lat <> "," <> lon,
        reg = CoreAddr.state address,
        zip = Just $ CoreAddr.area_code address
      }

mkSearchItem :: Text -> QuoteRes -> Core.Item
mkSearchItem itemId QuoteRes {..} =
  Core.Item
    { id = itemId,
      parent_item_id = Nothing,
      descriptor = emptyDescriptor,
      price = price,
      model_id = Nothing,
      category_id = Nothing,
      package_category_id = Nothing,
      brand_id = Nothing,
      promotional = False,
      ttl = Nothing,
      tags = [Tag "eta" (T.pack $ show eta)]
    }
  where
    price =
      Price
        { currency = "INR",
          value = Nothing,
          estimated_value = Just value,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }
    value = convertAmountToDecimalValue (Amount $ toRational pricing)

mkQuote :: QuoteRes -> Flow Quotation
mkQuote QuoteRes {..} = do
  qid <- generateGUID
  return $ Quotation {id = qid, price = Just price, ttl = Nothing, breakup = Nothing}
  where
    price = mkPrice pricing
    mkPrice estimatedPrice =
      Price
        { currency = "INR",
          value = Nothing,
          estimated_value = Just $ convertAmountToDecimalValue $ Amount $ toRational estimatedPrice,
          computed_value = Nothing,
          listed_value = Nothing,
          offered_value = Nothing,
          minimum_value = Nothing,
          maximum_value = Nothing
        }

mkPayment :: PaymentEndpoint -> Float -> Payment
mkPayment payee estimated_price =
  Payment
    { transaction_id = Nothing,
      _type = Just "PRE-FULFILLMENT",
      payer = Nothing,
      payee = Just payee,
      methods = ["RTGS"],
      amount = price,
      state = Nothing,
      due_date = Nothing,
      duration = Nothing
    }
  where
    price =
      MonetaryValue
        { currency = "INR",
          value = convertAmountToDecimalValue $ Amount $ toRational estimated_price
        }

updateBppUri :: Context -> BaseUrl -> Context
updateBppUri Context {..} bpNwAddress = Context {bpp_uri = Just bpNwAddress, ..}

getDlBAPCreds :: Organization -> Flow DlBAConfig
getDlBAPCreds = getClientConfig
