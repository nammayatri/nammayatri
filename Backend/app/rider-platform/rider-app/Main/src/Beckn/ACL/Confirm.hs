{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Confirm (buildConfirmReqV2) where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.Utils
import Control.Lens ((%~))
import qualified Data.Aeson as A
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnInit as DOnInit
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DLoc
import EulerHS.Prelude hiding (id, state, (%~))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

buildConfirmReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DOnInit.OnInitRes ->
  m Spec.ConfirmReq
buildConfirmReqV2 res = do
  messageId <- generateGUID
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack res.merchant.id.getId)
  -- TODO :: Add request city, after multiple city support on gateway.
  moc <- CQMOC.findByMerchantIdAndCity res.merchant.id res.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> res.merchant.id.getId <> "-city-" <> show res.city)
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId res.merchant.id "MOBILITY" moc.id
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show res.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  ttl <- bapConfig.confirmTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.CONFIRM Context.MOBILITY messageId (Just res.transactionId) res.merchant.bapId bapUrl (Just res.bppId) (Just res.bppUrl) res.city res.merchant.country (Just ttl)
  let message = mkConfirmMessageV2 res bapConfig
  pure $ Spec.ConfirmReq context message

mkConfirmMessageV2 :: DOnInit.OnInitRes -> DBC.BecknConfig -> Spec.ConfirmReqMessage
mkConfirmMessageV2 res bapConfig = do
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res bapConfig
    }

tfOrder :: DOnInit.OnInitRes -> DBC.BecknConfig -> Spec.Order
tfOrder res bapConfig = do
  Spec.Order
    { orderBilling = tfOrderBilling res,
      orderCancellation = Nothing,
      orderCancellationTerms = Nothing,
      orderFulfillments = tfFulfillments res,
      orderId = res.bppBookingId >>= Just . (.getId),
      orderItems = tfItems res,
      orderPayments = tfPayments res bapConfig,
      orderProvider = tfProvider res,
      orderQuote = Nothing, -- Not allowed in confirm request per ONDC spec
      orderStatus = Nothing,
      orderCreatedAt = Nothing,
      orderUpdatedAt = Nothing
    }

tfFulfillments :: DOnInit.OnInitRes -> Maybe [Spec.Fulfillment]
tfFulfillments res =
  Just
    [ Spec.Fulfillment
        { fulfillmentAgent = Nothing,
          fulfillmentCustomer = tfCustomer res,
          fulfillmentId = res.fulfillmentId,
          fulfillmentState = Nothing,
          fulfillmentStops = mkStopsForConfirm (Just res.fromLocation) stops res.mbToLocation,
          fulfillmentTags = Nothing,
          fulfillmentType = Utils.tripCategoryToFulfillmentType <$> res.tripCategory,
          fulfillmentVehicle = tfVehicle res
        }
    ]
  where
    stops = case res.bookingDetails of
      DRB.OneWayDetails details -> details.stops
      DRB.OneWaySpecialZoneDetails details -> details.stops
      _ -> []

tfItems :: DOnInit.OnInitRes -> Maybe [Spec.Item]
tfItems res =
  Just
    [ Spec.Item
        { itemDescriptor = Nothing,
          itemFulfillmentIds = Nothing,
          itemId = Just res.itemId,
          itemLocationIds = Nothing,
          itemPaymentIds = Nothing,
          itemPrice = Nothing,
          itemTags = Nothing
        }
    ]

-- | Build payment for confirm request
-- Per ONDC spec, confirm request should not include currency in payment params
tfPayments :: DOnInit.OnInitRes -> DBC.BecknConfig -> Maybe [Spec.Payment]
tfPayments res bapConfig = do
  let mPrice = Just res.estimatedTotalFare
  let mkParams :: (Maybe BknPaymentParams) = decodeFromText =<< bapConfig.paymentParamsJson
  Just $ DL.singleton $ mkPaymentForConfirm (show res.city) (show bapConfig.collectedBy) Enums.NOT_PAID mPrice res.paymentId mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee

tfCustomer :: DOnInit.OnInitRes -> Maybe Spec.Customer
tfCustomer res =
  Just $
    Spec.Customer
      { customerContact = mkContact,
        customerPerson = mkPerson
      }
  where
    mkContact = do
      let trimCountryCode number = fromMaybe number (T.stripPrefix "+91" number)
      Just $
        Spec.Contact
          { contactPhone = Just $ trimCountryCode res.riderPhoneNumber
          -- handling of passing virtual number at on_init domain handler.
          }

    mkPerson = do
      return $
        Spec.Person
          { personId = Nothing,
            personImage = Nothing,
            personName = res.mbRiderName,
            personTags = mkPersonTags
          }

    mkPersonTags
      | not res.isValueAddNP = Nothing
      | otherwise =
        Just
          [ Spec.TagGroup
              { tagGroupDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.CUSTOMER_INFO,
                        descriptorName = Just "Customer Information",
                        descriptorShortDesc = Nothing
                      },
                tagGroupDisplay = Just False,
                tagGroupList = mkPersonTag
              }
          ]

    mkPersonTag =
      Just
        [ Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.NIGHT_SAFETY_CHECK,
                      descriptorName = Just "Night Safety Check",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.nightSafetyCheck
            },
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.ENABLE_FREQUENT_LOCATION_UPDATES,
                      descriptorName = Just "Enable Frequent Location Updates",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.enableFrequentLocationUpdates
            },
          Spec.Tag
            { tagDescriptor =
                Just $
                  Spec.Descriptor
                    { descriptorCode = Just $ show Tags.ENABLE_OTP_LESS_RIDE,
                      descriptorName = Just "Enable OTP Less Ride",
                      descriptorShortDesc = Nothing
                    },
              tagDisplay = Just False,
              tagValue = Just $ show res.enableOtpLessRide
            }
        ]

tfVehicle :: DOnInit.OnInitRes -> Maybe Spec.Vehicle
tfVehicle res = do
  let (category, variant) = Utils.castVehicleVariant res.vehicleVariant
  Just $
    Spec.Vehicle
      { vehicleCategory = Just category,
        vehicleVariant = Just variant,
        vehicleColor = Nothing,
        vehicleMake = Nothing,
        vehicleModel = Nothing,
        vehicleRegistration = Nothing,
        vehicleCapacity = Nothing
      }

tfProvider :: DOnInit.OnInitRes -> Maybe Spec.Provider
tfProvider res =
  Just $
    Spec.Provider
      { providerId = Just res.bppId,
        providerItems = Nothing,
        providerLocations = Nothing,
        providerPayments = Nothing,
        providerDescriptor = Nothing,
        providerFulfillments = Nothing
      }

tfOrderBilling :: DOnInit.OnInitRes -> Maybe Spec.Billing
tfOrderBilling res =
  Just $
    Spec.Billing
      { billingPhone = Just $ maskNumber res.riderPhoneNumber,
        billingName = res.mbRiderName
      }

-- | Build payment for confirm request without currency in params
-- Per ONDC spec, confirm request should not include currency in payment params
mkPaymentForConfirm ::
  Text -> -- City
  Text -> -- CollectedBy
  Enums.PaymentStatus ->
  Maybe Price ->
  Maybe Text -> -- TxnId
  Maybe BknPaymentParams ->
  Maybe Text -> -- SettlementType
  Maybe Text -> -- SettlementWindow
  Maybe BaseUrl -> -- StaticTermsUrl
  Maybe Text -> -- BuyerFinderFee
  Spec.Payment
mkPaymentForConfirm txnCity collectedBy paymentStatus mPrice mTxnId mPaymentParams mSettlementType mSettlementWindow mSettlementTermsUrl mbff = do
  let mAmount = show . (.amount) <$> mPrice
  Spec.Payment
    { paymentCollectedBy = Just collectedBy,
      paymentId = mTxnId,
      paymentParams =
        if Kernel.Prelude.or [isJust mTxnId, isJust mAmount, isJust mPaymentParams]
          then Just $ mkPaymentParamsForConfirm mPaymentParams mTxnId mAmount
          else Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTagsForConfirm txnCity mSettlementType mAmount mSettlementWindow mSettlementTermsUrl mbff,
      paymentType = encodeToText' Enums.ON_FULFILLMENT
    }

mkPaymentParamsForConfirm :: Maybe BknPaymentParams -> Maybe Text -> Maybe Text -> Spec.PaymentParams
mkPaymentParamsForConfirm mPaymentParams _mTxnId mAmount = do
  Spec.PaymentParams
    { paymentParamsAmount = mAmount,
      paymentParamsBankAccountNumber = mPaymentParams >>= (.bankAccNumber),
      paymentParamsBankCode = mPaymentParams >>= (.bankCode),
      paymentParamsCurrency = Nothing, -- Not allowed in confirm request per ONDC spec
      paymentParamsVirtualPaymentAddress = mPaymentParams >>= (.vpa)
    }

mkPaymentTagsForConfirm :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe BaseUrl -> Maybe Text -> [Spec.TagGroup]
mkPaymentTagsForConfirm _txnCity mSettlementType mAmount mSettlementWindow mSettlementTermsUrl mbff =
  catMaybes
    [ Just $ mkBuyerFinderFeeTagGroup mbff,
      Just $ mkSettlementTagGroup mSettlementType mAmount mSettlementWindow mSettlementTermsUrl
    ]

mkBuyerFinderFeeTagGroup :: Maybe Text -> Spec.TagGroup
mkBuyerFinderFeeTagGroup mbff =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.BUYER_FINDER_FEES,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.BUYER_FINDER_FEES_PERCENTAGE,
                        descriptorName = Nothing,
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Nothing, -- Not allowed in confirm request per ONDC spec
                tagValue = mbff
              }
          ]
    }

mkSettlementTagGroup :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe BaseUrl -> Spec.TagGroup
mkSettlementTagGroup mSettlementType mAmount mSettlementWindow mSettlementTermsUrl =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.SETTLEMENT_TERMS,
              descriptorName = Nothing,
              descriptorShortDesc = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList =
        Just $
          catMaybes
            [ Just $
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.SETTLEMENT_TYPE,
                            descriptorName = Nothing,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = Nothing, -- Not allowed in confirm request per ONDC spec
                    tagValue = mSettlementType
                  },
              ( \amount ->
                  Spec.Tag
                    { tagDescriptor =
                        Just $
                          Spec.Descriptor
                            { descriptorCode = Just $ show Tags.SETTLEMENT_AMOUNT,
                              descriptorName = Nothing,
                              descriptorShortDesc = Nothing
                            },
                      tagDisplay = Nothing, -- Not allowed in confirm request per ONDC spec
                      tagValue = Just amount
                    }
              )
                <$> mAmount,
              Just $
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.SETTLEMENT_WINDOW,
                            descriptorName = Nothing,
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = Nothing, -- Not allowed in confirm request per ONDC spec
                    tagValue = mSettlementWindow
                  },
              ( \url ->
                  Spec.Tag
                    { tagDescriptor =
                        Just $
                          Spec.Descriptor
                            { descriptorCode = Just $ show Tags.STATIC_TERMS,
                              descriptorName = Nothing,
                              descriptorShortDesc = Nothing
                            },
                      tagDisplay = Nothing, -- Not allowed in confirm request per ONDC spec
                      tagValue = Just $ showBaseUrl url
                    }
              )
                <$> mSettlementTermsUrl
            ]
    }

-- | Helper function to encode values to JSON text
encodeToText' :: (A.ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode

-- | Build stops for confirm request without address field
-- Per ONDC spec, confirm request should not include location.address in stops
mkStopsForConfirm :: Maybe DLoc.Location -> [DLoc.Location] -> Maybe DLoc.Location -> Maybe [Spec.Stop]
mkStopsForConfirm mOrigin intermediateStops mDestination =
  let originGps origin = Gps.Gps {lat = origin.lat, lon = origin.lon}
      destinationGps dest = Gps.Gps {lat = dest.lat, lon = dest.lon}
   in Just $
        catMaybes
          [ ( \origin ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationAreaCode = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationCity = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationCountry = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationGps = Utils.gpsToText (originGps origin),
                            locationState = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationId = Nothing,
                            locationUpdatedAt = Nothing
                          },
                    stopType = Just $ show Enums.START,
                    stopAuthorization = Nothing,
                    stopTime = Nothing,
                    stopId = Nothing, -- Not allowed in confirm request per ONDC spec
                    stopParentStopId = Nothing
                  }
            )
              <$> mOrigin,
            ( \destination ->
                Spec.Stop
                  { stopLocation =
                      Just $
                        Spec.Location
                          { locationAddress = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationAreaCode = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationCity = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationCountry = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationGps = Utils.gpsToText (destinationGps destination),
                            locationState = Nothing, -- Not allowed in confirm request per ONDC spec
                            locationId = Nothing,
                            locationUpdatedAt = Nothing
                          },
                    stopType = Just $ show Enums.END,
                    stopAuthorization = Nothing,
                    stopTime = Nothing,
                    stopId = Nothing, -- Not allowed in confirm request per ONDC spec
                    stopParentStopId = Nothing
                  }
            )
              <$> mDestination
          ]
          <> fmap (\(location, stopId) -> mkIntermediateStopForConfirm location stopId (stopId - 1)) (zip intermediateStops [1 ..])

mkIntermediateStopForConfirm :: DLoc.Location -> Int -> Int -> Spec.Stop
mkIntermediateStopForConfirm stop _stopId _parentStopId =
  let gps = Gps.Gps {lat = stop.lat, lon = stop.lon}
   in Spec.Stop
        { stopLocation =
            Just $
              Spec.Location
                { locationAddress = Nothing, -- Not allowed in confirm request per ONDC spec
                  locationAreaCode = Nothing, -- Not allowed in confirm request per ONDC spec
                  locationCity = Nothing, -- Not allowed in confirm request per ONDC spec
                  locationCountry = Nothing, -- Not allowed in confirm request per ONDC spec
                  locationGps = Utils.gpsToText gps,
                  locationState = Nothing, -- Not allowed in confirm request per ONDC spec
                  locationId = Just stop.id.getId,
                  locationUpdatedAt = Nothing
                },
          stopType = Just $ show Enums.INTERMEDIATE_STOP,
          stopAuthorization = Nothing,
          stopTime = Nothing,
          stopId = Nothing, -- Not allowed in confirm request per ONDC spec
          stopParentStopId = Nothing
        }
