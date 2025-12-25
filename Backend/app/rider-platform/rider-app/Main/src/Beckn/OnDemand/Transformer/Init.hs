module Beckn.OnDemand.Transformer.Init where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Init
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context
import BecknV2.Utils (maskNumber)
import qualified Data.List
import qualified Data.Text
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.Trip as Trip
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Utils.Text
import qualified SharedLogic.Confirm

buildInitReq :: (Kernel.Types.App.MonadFlow m) => SharedLogic.Confirm.DConfirmRes -> Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Bool -> DBC.BecknConfig -> DRC.RiderConfig -> Text -> m BecknV2.OnDemand.Types.InitReq
buildInitReq uiConfirm bapUrl action domain isValueAddNP bapConfig riderConfig initTtl = do
  initReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain uiConfirm.booking.id.getId (Just uiConfirm.searchRequestId.getId) uiConfirm.merchant.bapId bapUrl (Just uiConfirm.providerId) (Just uiConfirm.providerUrl) uiConfirm.city uiConfirm.merchant.country (Just initTtl)
  let initReqMessage_ = buildInitReqMessage uiConfirm isValueAddNP bapConfig riderConfig
  pure $ BecknV2.OnDemand.Types.InitReq {initReqContext = initReqContext_, initReqMessage = initReqMessage_}

buildInitReqMessage :: SharedLogic.Confirm.DConfirmRes -> Bool -> DBC.BecknConfig -> DRC.RiderConfig -> BecknV2.OnDemand.Types.ConfirmReqMessage
buildInitReqMessage uiConfirm isValueAddNP bapConfig riderConfig = do
  let confirmReqMessageOrder_ = tfOrder uiConfirm isValueAddNP bapConfig riderConfig
  BecknV2.OnDemand.Types.ConfirmReqMessage {confirmReqMessageOrder = confirmReqMessageOrder_}

tfFulfillmentVehicle :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Vehicle
tfFulfillmentVehicle uiConfirm = do
  let (category, variant) = Beckn.OnDemand.Utils.Common.castVehicleVariant uiConfirm.vehicleVariant
  let vehicleCategory_ = Just category
  let vehicleColor_ = Nothing
  let vehicleMake_ = Nothing
  let vehicleModel_ = Nothing
  let vehicleRegistration_ = Nothing
  let vehicleVariant_ = Just variant
  let vehicleCapacity_ = Nothing
  BecknV2.OnDemand.Types.Vehicle {vehicleCategory = vehicleCategory_, vehicleColor = vehicleColor_, vehicleMake = vehicleMake_, vehicleModel = vehicleModel_, vehicleRegistration = vehicleRegistration_, vehicleVariant = vehicleVariant_, vehicleCapacity = vehicleCapacity_}

tfOrder :: SharedLogic.Confirm.DConfirmRes -> Bool -> DBC.BecknConfig -> DRC.RiderConfig -> BecknV2.OnDemand.Types.Order
tfOrder uiConfirm isValueAddNP bapConfig riderConfig = do
  let orderCancellation_ = Nothing
  let orderCancellationTerms_ = Nothing
  let orderId_ = Nothing
  let orderProvider_ = Just $ tfProvider uiConfirm
  let orderPayments_ = Beckn.OnDemand.Utils.Init.mkPayment uiConfirm.paymentMethodInfo bapConfig riderConfig uiConfirm.city uiConfirm.isStripe uiConfirm.paymentMode & Just
  let orderStatus_ = Nothing
  let orderQuote_ = Nothing
  let orderBilling_ = tfOrderBilling uiConfirm.riderPhone uiConfirm.riderName & Just
  let orderFulfillments_ = Just $ Data.List.singleton $ tfOrderFulfillments uiConfirm isValueAddNP
  let orderItems_ = Just $ Data.List.singleton $ tfOrderItems uiConfirm
  BecknV2.OnDemand.Types.Order {orderBilling = orderBilling_, orderCancellation = orderCancellation_, orderCancellationTerms = orderCancellationTerms_, orderFulfillments = orderFulfillments_, orderId = orderId_, orderItems = orderItems_, orderPayments = orderPayments_, orderProvider = orderProvider_, orderQuote = orderQuote_, orderStatus = orderStatus_, orderCreatedAt = Just uiConfirm.booking.createdAt, orderUpdatedAt = Just uiConfirm.booking.updatedAt}

tfOrderBilling :: Maybe Data.Text.Text -> Maybe Data.Text.Text -> BecknV2.OnDemand.Types.Billing
tfOrderBilling mbPhoneNumber mbRiderName = do
  let billingPhone_ = mbPhoneNumber <&> maskNumber
  BecknV2.OnDemand.Types.Billing {billingPhone = billingPhone_, billingName = mbRiderName}

tfOrderFulfillments :: SharedLogic.Confirm.DConfirmRes -> Bool -> BecknV2.OnDemand.Types.Fulfillment
tfOrderFulfillments uiConfirm isValueAddNP = do
  let fulfillmentAgent_ = Nothing
  let fulfillmentCustomer_ = tfCustomer uiConfirm
  let fulfillmentId_ = Just uiConfirm.bppQuoteId
  let fulfillmentState_ = Nothing
  let fulfillmentStops_ = Beckn.OnDemand.Utils.Init.mkStops uiConfirm.fromLoc uiConfirm.toLoc Nothing uiConfirm.stops
  let fulfillmentTags_ = if isValueAddNP then Beckn.OnDemand.Utils.Init.mkFulfillmentTags uiConfirm.maxEstimatedDistance else Nothing
  let fulfillmentType_ = Utils.tripCategoryToFulfillmentType <$> uiConfirm.booking.tripCategory
  let fulfillmentVehicle_ = tfFulfillmentVehicle uiConfirm & Just
  BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}

tfOrderItems :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Item
tfOrderItems uiConfirm = do
  let itemDescriptor_ = Nothing
  let itemFulfillmentIds_ = Just $ Data.List.singleton uiConfirm.bppQuoteId
  let itemId_ = Just uiConfirm.itemId
  let itemLocationIds_ = Nothing
  let itemPaymentIds_ = Nothing
  let itemPrice_ = Nothing
  let itemTags_ = Just $ mkItemTags uiConfirm
  BecknV2.OnDemand.Types.Item {itemDescriptor = itemDescriptor_, itemFulfillmentIds = itemFulfillmentIds_, itemId = itemId_, itemLocationIds = itemLocationIds_, itemPaymentIds = itemPaymentIds_, itemPrice = itemPrice_, itemTags = itemTags_}

tfPrice :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Price
tfPrice uiConfirm = do
  let priceComputedValue_ = Nothing
  let priceCurrency_ = Nothing
  let priceMaximumValue_ = Nothing
  let priceMinimumValue_ = Nothing
  let priceOfferedValue_ = Kernel.Utils.Text.encodeToText uiConfirm.booking.estimatedTotalFare.amount & Just
  let priceValue_ = Kernel.Utils.Text.encodeToText uiConfirm.booking.estimatedTotalFare.amount & Just
  BecknV2.OnDemand.Types.Price {priceComputedValue = priceComputedValue_, priceCurrency = priceCurrency_, priceMaximumValue = priceMaximumValue_, priceMinimumValue = priceMinimumValue_, priceOfferedValue = priceOfferedValue_, priceValue = priceValue_}

tfProvider :: SharedLogic.Confirm.DConfirmRes -> BecknV2.OnDemand.Types.Provider
tfProvider uiConfirm = do
  let providerId = Just uiConfirm.providerId
      providerItems = Nothing
      providerLocations = Nothing
      providerPayments = Nothing
      providerDescriptor = Nothing
      providerFulfillments = Nothing

  BecknV2.OnDemand.Types.Provider {..}

tfCustomer :: SharedLogic.Confirm.DConfirmRes -> Maybe BecknV2.OnDemand.Types.Customer
tfCustomer res =
  Just $
    BecknV2.OnDemand.Types.Customer
      { customerContact = mkContact,
        customerPerson = mkPerson
      }
  where
    mkContact = do
      let trimCountryCode mbNumber = mbNumber <&> \number -> fromMaybe number (Data.Text.stripPrefix "+91" number)
      Just $
        BecknV2.OnDemand.Types.Contact
          { contactPhone = trimCountryCode res.riderPhone
          -- handling of passing virtual number at UIconfirm domain handler.
          }

    mkPerson = do
      riderName <- res.riderName
      return $
        BecknV2.OnDemand.Types.Person
          { personId = Nothing,
            personImage = Nothing,
            personName = Just riderName,
            personTags = Nothing
          }

mkItemTags :: SharedLogic.Confirm.DConfirmRes -> [Spec.TagGroup]
mkItemTags res =
  let itemTags = if maybe False Trip.isDeliveryTrip res.booking.tripCategory then mkDeliveryTagGroup res else []
      itemTags' = mkAdvancedBookingEnabledTagGroup res : itemTags
      itemTags'' = mkInsuranceTagGroup res : itemTags'
   in itemTags''

mkAdvancedBookingEnabledTagGroup :: SharedLogic.Confirm.DConfirmRes -> Spec.TagGroup
mkAdvancedBookingEnabledTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.FORWARD_BATCHING_REQUEST_INFO,
              descriptorName = Just "Forward Batch Enabled",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.IS_FORWARD_BATCH_ENABLED,
                        descriptorName = Just "Forward Batch Enabled",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show res.isAdvanceBookingEnabled
              }
          ]
    }

mkInsuranceTagGroup :: SharedLogic.Confirm.DConfirmRes -> Spec.TagGroup
mkInsuranceTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.INSURANCE_INFO,
              descriptorName = Just "Insurance Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.IS_INSURED,
                        descriptorName = Just "Is Insured",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show $ fromMaybe False res.isInsured
              },
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.INSURED_AMOUNT,
                        descriptorName = Just "Insured Amount",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = res.insuredAmount
              }
          ]
    }

mkDeliveryTagGroup :: SharedLogic.Confirm.DConfirmRes -> [Spec.TagGroup]
mkDeliveryTagGroup res =
  maybe
    []
    ( \(SharedLogic.Confirm.DConfirmResDelivery (DTDD.DeliveryDetails {..})) ->
        pure $
          Spec.TagGroup
            { tagGroupDisplay = Just False,
              tagGroupDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.DELIVERY,
                      descriptorName = Just "Delivery Info",
                      descriptorShortDesc = Nothing
                    },
              tagGroupList =
                Just
                  [ Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.INITIATED_AS,
                                descriptorName = Just "Delivery Initiated As",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just $ show initiatedAs
                      },
                    Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.SENDER_NAME,
                                descriptorName = Just "Sender Name",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just senderDetails.name
                      },
                    Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.SENDER_LOCATION_INSTRUCTIONS,
                                descriptorName = Just "Sender Location Instructions",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just $ mkLocationInstructions senderDetails.address.instructions senderDetails.address.extras
                      },
                    Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.SENDER_NUMBER,
                                descriptorName = Just "Sender phone number",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just senderDetails.phoneNumber
                      },
                    Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.RECEIVER_NAME,
                                descriptorName = Just "Receiver Name",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just receiverDetails.name
                      },
                    Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.RECEIVER_LOCATION_INSTRUCTIONS,
                                descriptorName = Just "Receiver Location Instructions",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just $ mkLocationInstructions receiverDetails.address.instructions receiverDetails.address.extras
                      },
                    Spec.Tag
                      { tagDescriptor =
                          Just $
                            Spec.Descriptor
                              { descriptorCode = Just $ show Tags.RECEIVER_NUMBER,
                                descriptorName = Just "Receiver phone number",
                                descriptorShortDesc = Nothing
                              },
                        tagDisplay = Just False,
                        tagValue = Just receiverDetails.phoneNumber
                      }
                  ]
            }
    )
    res.confirmResDetails
  where
    mkLocationInstructions instructions extras =
      fromMaybe mempty instructions
        <> "|"
        <> fromMaybe mempty extras
