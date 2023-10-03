module Beckn.ACL.Bus.OnInit (buildOnInitBusReq) where

import Beckn.ACL.Common (getTag)
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import qualified Beckn.Types.Core.Taxi.OnInit as OnInit
import Data.Text
import qualified Domain.Action.Beckn.OnInit as DOnInit
import EulerHS.Prelude hiding (find, id, map, state, unpack)
import Kernel.Prelude
import Kernel.Product.Validation.Context (validateBusContext)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error

buildOnInitBusReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnInit.OnInitReq ->
  m (Maybe DOnInit.OnInitReq)
buildOnInitBusReq req = do
  validateBusContext Context.ON_INIT $ req.context
  handleErrorFRFS req.contents $ \message -> do
    let fulfillment = message.order.fulfillment
    tagGroups <- fulfillment.tags & fromMaybeM (InvalidRequest "Missing fulfillment.tags")
    fareBreakup <- buildFareBreakup tagGroups
    let fareBreakupsList = convertToFareBreakupList fareBreakup
    logDebug $ "fareBreakUp : " <> show fareBreakup
    let ticketId = Just $ Id req.context.message_id
        estimatedFare = message.order.quote.price.value
        bppBookingId = Nothing
        bookingId = Nothing
    let discount = Money 0
    return $
      DOnInit.OnInitReq
        { estimatedFare = roundToIntegral estimatedFare,
          estimatedTotalFare = roundToIntegral estimatedFare,
          discount = Just discount,
          paymentUrl = message.order.payment.uri,
          fareBreakup = Just fareBreakupsList,
          ..
        }

convertToFareBreakupList :: DOnInit.FareBreakup -> [DOnInit.OnInitFareBreakup]
convertToFareBreakupList DOnInit.FareBreakup {..} =
  [ DOnInit.OnInitFareBreakup (readTextAsMoney totalFare) "Total Fare",
    DOnInit.OnInitFareBreakup (readTextAsMoney basicFare) "Basic Fare",
    DOnInit.OnInitFareBreakup (readTextAsMoney discount) "Discount",
    DOnInit.OnInitFareBreakup (readTextAsMoney cgst) "CGST",
    DOnInit.OnInitFareBreakup (readTextAsMoney sgst) "SGST"
  ]
  where
    readTextAsMoney :: Text -> HighPrecMoney
    readTextAsMoney txt = HighPrecMoney $ read (unpack txt)

handleErrorFRFS ::
  (MonadFlow m) =>
  Either Error OnInit.OnInitMessage ->
  (OnInit.OnInitMessage -> m DOnInit.OnInitReq) ->
  m (Maybe DOnInit.OnInitReq)
handleErrorFRFS etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_init req" $ "on_init error: " <> show err
      pure Nothing

buildFareBreakup :: (MonadThrow m, Log m) => OnInit.TagGroups -> m DOnInit.FareBreakup
buildFareBreakup tagGroups = do
  totalFare <- getTotalFare tagGroups
  basicFare <- getBasicFare tagGroups
  discount <- getDiscount tagGroups
  cgst <- getCGST tagGroups
  sgst <- getSGST tagGroups
  pure $ DOnInit.FareBreakup {..}

getTotalFare :: (MonadThrow m, Log m) => OnInit.TagGroups -> m Text
getTotalFare tagGroups = do
  let mbTotalFare = getTag fareBreakupTagGroupCode totalFareTagCode tagGroups
  mbTotalFare & fromMaybeM (InvalidRequest $ buildErrorMessage totalFareTagCode fareBreakupTagGroupCode)

getBasicFare :: (MonadThrow m, Log m) => OnInit.TagGroups -> m Text
getBasicFare tagGroups = do
  let mbBasicFare = getTag fareBreakupTagGroupCode basicFareTagCode tagGroups
  mbBasicFare & fromMaybeM (InvalidRequest $ buildErrorMessage totalFareTagCode fareBreakupTagGroupCode)

getDiscount :: (MonadThrow m, Log m) => OnInit.TagGroups -> m Text
getDiscount tagGroups = do
  let mbDiscount = getTag fareBreakupTagGroupCode discountTagCode tagGroups
  mbDiscount & fromMaybeM (InvalidRequest $ buildErrorMessage discountTagCode fareBreakupTagGroupCode)

getCGST :: (MonadThrow m, Log m) => OnInit.TagGroups -> m Text
getCGST tagGroups = do
  let mbCGST = getTag fareBreakupTagGroupCode cgstTagCode tagGroups
  mbCGST & fromMaybeM (InvalidRequest $ buildErrorMessage cgstTagCode fareBreakupTagGroupCode)

getSGST :: (MonadThrow m, Log m) => OnInit.TagGroups -> m Text
getSGST tagGroups = do
  let mbSGST = getTag fareBreakupTagGroupCode sgstTagCode tagGroups
  mbSGST & fromMaybeM (InvalidRequest $ buildErrorMessage sgstTagCode fareBreakupTagGroupCode)

buildErrorMessage :: TagCode -> TagGroupCode -> Text
buildErrorMessage tagCode tagGroupCode =
  "Missing \'" <> tagCode <> "\' tag in \'" <> tagGroupCode <> "\' TagGroup in fulfillment.tags"

type TagGroupCode = Text

type TagCode = Text

fareBreakupTagGroupCode :: TagGroupCode
fareBreakupTagGroupCode = "Fare Breakup"

totalFareTagCode :: TagCode
totalFareTagCode = "Total Fare"

basicFareTagCode :: TagCode
basicFareTagCode = "Basic Fare"

discountTagCode :: TagCode
discountTagCode = "Discount"

cgstTagCode :: TagCode
cgstTagCode = "CGST"

sgstTagCode :: TagCode
sgstTagCode = "SGST"
