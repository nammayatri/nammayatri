module Beckn.OnDemand.Transformer.Init where

import qualified Beckn.OnDemand.Utils.Init
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.Utils as Utils
import Control.Lens ((^?), _Just, _head)
import qualified Data.Aeson as A
import qualified Data.Maybe
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Init
import Domain.Types
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.Location as Location
import qualified Domain.Types.Trip as Trip
import EulerHS.Prelude hiding (id, (^?))
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber
import qualified Kernel.Utils.Common

buildDInitReq :: (Kernel.Types.App.MonadFlow m) => Kernel.Types.Registry.Subscriber.Subscriber -> BecknV2.OnDemand.Types.InitReq -> Bool -> m Domain.Action.Beckn.Init.InitReq
buildDInitReq subscriber req isValueAddNP = do
  let bapId_ = subscriber.subscriber_id
  -- Use contextBapUri if present, otherwise fall back to subscriber.subscriber_url
  bapUri_ <- case req.initReqContext.contextBapUri of
    Just bapUriText -> Kernel.Prelude.parseBaseUrl bapUriText
    Nothing -> pure subscriber.subscriber_url
  bapCityText <- req.initReqContext.contextLocation >>= (.locationCity) >>= (.cityCode) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't find City")
  bapCity_ <- A.decode (A.encode bapCityText) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't parse City")
  bapCountryText <- req.initReqContext.contextLocation >>= (.locationCountry) >>= (.countryCode) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't find Country")
  bapCountry_ <- A.decode (A.encode bapCountryText) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Couldn't parse Country")
  fulfillmentId__ <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentId) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "FulfillmentId not found. It should either be estimateId or quoteId")
  tripCategory <- if isValueAddNP then req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentType) >>= (Just . BecknV2.OnDemand.Utils.Common.fulfillmentTypeToTripCategory) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "FulfillmentType not found") else pure (req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentType) >>= (Just . BecknV2.OnDemand.Utils.Common.fulfillmentTypeToTripCategory) & Data.Maybe.fromMaybe (OneWay OneWayOnDemandDynamicOffer))
  let fulfillmentId_ =
        case tripCategoryToPricingPolicy tripCategory of
          EstimateBased _ -> Domain.Action.Beckn.Init.DriverQuoteId (Kernel.Types.Id.Id fulfillmentId__)
          QuoteBased _ -> Domain.Action.Beckn.Init.QuoteId (Kernel.Types.Id.Id fulfillmentId__)
  let maxEstimatedDistance_ = req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentTags) >>= Beckn.OnDemand.Utils.Init.getMaxEstimateDistance
  paymentMethodInfo_ <- req.initReqMessage.confirmReqMessageOrder.orderPayments ^? _Just . _head & Kernel.Prelude.mapM Beckn.OnDemand.Utils.Init.mkPaymentMethodInfo <&> Kernel.Prelude.join
  let paymentMode = req.initReqMessage.confirmReqMessageOrder.orderPayments ^? _Just . _head <&> Beckn.OnDemand.Utils.Init.mkPaymentMode & Kernel.Prelude.join
  let vehCategory = req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentVehicle) >>= (.vehicleCategory)
      vehVariant = req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentVehicle) >>= (.vehicleVariant)
  vehicleVariant_ <- Beckn.OnDemand.Utils.Init.castVehicleVariant vehCategory vehVariant & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest $ "Unable to parse vehicle variant:-" <> show vehVariant <> ",vehicle category:-" <> show vehCategory)
  let bppSubscriberId_ = req.initReqContext.contextBppId
  riderPhoneNumber <- req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentCustomer) >>= (.customerContact) >>= (.contactPhone) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Customer Phone not found.")
  let mbRiderName = req.initReqMessage.confirmReqMessageOrder.orderFulfillments ^? _Just . _head >>= (.fulfillmentCustomer) >>= (.customerPerson) >>= (.personName)
  estimateId <- req.initReqMessage.confirmReqMessageOrder.orderItems ^? _Just . _head >>= (.itemId) & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Item Id not found.")
  orderItem <- req.initReqMessage.confirmReqMessageOrder.orderItems ^? _Just . _head & Kernel.Utils.Common.fromMaybeM (Kernel.Types.Error.InvalidRequest "Order Item not found.")
  let initReqDetails = case tripCategory of
        Delivery _ -> getDeliveryDetails orderItem.itemTags
        _ -> Nothing
  let isAdvanceBookingEnabled = Just $ getAdvancedBookingEnabled orderItem.itemTags
  let (isInsured, insuredAmount) = getIsInsured orderItem.itemTags
  let displayBookingId = getDisplayBookingId orderItem.itemTags
  pure $ Domain.Action.Beckn.Init.InitReq {bapCity = bapCity_, bapCountry = bapCountry_, bapId = bapId_, bapUri = bapUri_, fulfillmentId = fulfillmentId_, maxEstimatedDistance = maxEstimatedDistance_, paymentMethodInfo = paymentMethodInfo_, vehicleVariant = vehicleVariant_, bppSubscriberId = bppSubscriberId_, estimateId = estimateId, ..}

getDeliveryDetails :: Maybe [Spec.TagGroup] -> Maybe Domain.Action.Beckn.Init.InitReqDetails
getDeliveryDetails tagGroups = do
  initiatedAs <- Utils.getTagV2 Tag.DELIVERY Tag.INITIATED_AS tagGroups
  senderName <- Utils.getTagV2 Tag.DELIVERY Tag.SENDER_NAME tagGroups
  senderPhone <- Utils.getTagV2 Tag.DELIVERY Tag.SENDER_NUMBER tagGroups
  senderLocIns <- Utils.getTagV2 Tag.DELIVERY Tag.SENDER_LOCATION_INSTRUCTIONS tagGroups
  receiverName <- Utils.getTagV2 Tag.DELIVERY Tag.RECEIVER_NAME tagGroups
  receiverPhone <- Utils.getTagV2 Tag.DELIVERY Tag.RECEIVER_NUMBER tagGroups
  receiverLocIns <- Utils.getTagV2 Tag.DELIVERY Tag.RECEIVER_LOCATION_INSTRUCTIONS tagGroups
  let (senderInstructions, senderAddressExtra) = splitInstructions senderLocIns
      (receiverInstructions, receiverAddressExtra) = splitInstructions receiverLocIns
      initiatedAsEnum = fromMaybe (Trip.DeliveryParty Trip.Sender) (readMaybe @(Trip.TripParty) $ T.unpack initiatedAs)
  pure $
    Domain.Action.Beckn.Init.InitReqDeliveryDetails $
      DTDD.DeliveryDetails
        { DTDD.senderDetails =
            DTDD.PersonDetails
              { DTDD.name = senderName,
                DTDD.phoneNumber = senderPhone,
                DTDD.address =
                  Location.LocationAddress
                    { Location.extras = senderAddressExtra,
                      Location.instructions = senderInstructions,
                      Location.area = Nothing,
                      Location.areaCode = Nothing,
                      Location.building = Nothing,
                      Location.city = Nothing,
                      Location.country = Nothing,
                      Location.door = Nothing,
                      Location.fullAddress = Nothing,
                      Location.state = Nothing,
                      Location.street = Nothing
                    }
              },
          DTDD.receiverDetails =
            DTDD.PersonDetails
              { DTDD.name = receiverName,
                DTDD.phoneNumber = receiverPhone,
                DTDD.address =
                  Location.LocationAddress
                    { Location.extras = receiverAddressExtra,
                      Location.instructions = receiverInstructions,
                      Location.area = Nothing,
                      Location.areaCode = Nothing,
                      Location.building = Nothing,
                      Location.city = Nothing,
                      Location.country = Nothing,
                      Location.door = Nothing,
                      Location.fullAddress = Nothing,
                      Location.state = Nothing,
                      Location.street = Nothing
                    }
              },
          DTDD.initiatedAs = initiatedAsEnum
        }
  where
    correctIns str = if T.null str then Nothing else Just str
    splitInstructions :: Text -> (Maybe Text, Maybe Text)
    splitInstructions ins = case T.splitOn "|" ins of
      [ins1, ins2] -> (correctIns ins1, correctIns ins2)
      _ -> (Nothing, Nothing)

getAdvancedBookingEnabled :: Maybe [Spec.TagGroup] -> Bool
getAdvancedBookingEnabled tagGroups =
  let tagValue = Utils.getTagV2 Tag.FORWARD_BATCHING_REQUEST_INFO Tag.IS_FORWARD_BATCH_ENABLED tagGroups
   in case tagValue of
        Just "True" -> True
        Just "False" -> False
        _ -> False

getIsInsured :: Maybe [Spec.TagGroup] -> (Maybe Bool, Maybe Text)
getIsInsured tagGroups =
  let tagValue = Utils.getTagV2 Tag.INSURANCE_INFO Tag.IS_INSURED tagGroups
      insuredAmount = Utils.getTagV2 Tag.INSURANCE_INFO Tag.INSURED_AMOUNT tagGroups
   in case tagValue of
        Just "True" -> (Just True, insuredAmount)
        Just "False" -> (Just False, Nothing)
        _ -> (Nothing, Nothing)

getDisplayBookingId :: Maybe [Spec.TagGroup] -> Maybe Text
getDisplayBookingId = Utils.getTagV2 Tag.BOOKING_INFO Tag.DISPLAY_BOOKING_ID
