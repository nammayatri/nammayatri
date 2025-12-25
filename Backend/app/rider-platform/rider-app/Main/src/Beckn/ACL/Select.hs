{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Select (buildSelectReqV2) where

import qualified Beckn.OnDemand.Utils.Common as UCommon
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UCommonV2
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import BecknV2.OnDemand.Utils.Payment
import Control.Lens ((%~))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Domain.Action.UI.Select as DSelect
import Domain.Types.BecknConfig
import qualified Domain.Types.Location as Location
import Domain.Types.RiderConfig
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MerchantPaymentMethod as SLMPM
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import Tools.Error

buildSelectReqV2 ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl], CacheFlow m r, EsqDBFlow m r) =>
  DSelect.DSelectRes ->
  m Spec.SelectReq
buildSelectReqV2 dSelectRes = do
  endLoc <- dSelectRes.searchRequest.toLocation & fromMaybeM (InternalError "To location address not found")
  moc <- CQMOC.findByMerchantIdAndCity dSelectRes.merchant.id dSelectRes.city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> dSelectRes.merchant.id.getId <> "-city-" <> show dSelectRes.city)
  riderConfig <- QRC.findByMerchantOperatingCityId moc.id Nothing >>= fromMaybeM (RiderConfigDoesNotExist moc.id.getId)
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId dSelectRes.merchant.id "MOBILITY" moc.id
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show dSelectRes.merchant.id.getId <> " merchantOperatingCityId " <> show moc.id.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  -- stops <- dSelectRes.searchRequest.stops
  messageId <- generateGUID
  let message = buildSelectReqMessage dSelectRes endLoc dSelectRes.isValueAddNP bapConfig riderConfig
      transactionId = dSelectRes.searchRequest.id.getId
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack dSelectRes.merchant.id.getId)
  ttl <- bapConfig.selectTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  context <- ContextV2.buildContextV2 Context.SELECT Context.MOBILITY messageId (Just transactionId) dSelectRes.merchant.bapId bapUrl (Just dSelectRes.providerId) (Just dSelectRes.providerUrl) dSelectRes.city dSelectRes.merchant.country (Just ttl)
  pure $ Spec.SelectReq {selectReqContext = context, selectReqMessage = message}

buildSelectReqMessage :: DSelect.DSelectRes -> Location.Location -> Bool -> BecknConfig -> RiderConfig -> Spec.ConfirmReqMessage
buildSelectReqMessage res endLoc isValueAddNP bapConfig riderConfig =
  Spec.ConfirmReqMessage
    { confirmReqMessageOrder = tfOrder res endLoc isValueAddNP bapConfig riderConfig
    }

tfOrder :: DSelect.DSelectRes -> Location.Location -> Bool -> BecknConfig -> RiderConfig -> Spec.Order
tfOrder res endLoc isValueAddNP bapConfig riderConfig =
  let orderBilling = Nothing
      orderCancellation = Nothing
      orderCancellationTerms = Nothing
      orderId = Nothing
      orderPayments = tfPayments res bapConfig riderConfig
      orderProvider = Just $ tfProvider res
      orderQuote = Nothing
      orderStatus = Nothing
      startLoc = res.searchRequest.fromLocation
      orderItem = tfOrderItem res isValueAddNP
      orderFulfillment = tfFulfillment res startLoc endLoc isValueAddNP res.searchRequest.stops
      orderCreatedAt = Nothing
      orderUpdatedAt = Nothing
   in Spec.Order
        { orderFulfillments = Just [orderFulfillment],
          orderItems = Just [orderItem],
          ..
        }

tfFulfillment :: DSelect.DSelectRes -> Location.Location -> Location.Location -> Bool -> [Location.Location] -> Spec.Fulfillment
tfFulfillment res startLoc endLoc isValueAddNP stops =
  let fulfillmentAgent = Nothing
      fulfillmentTags = Nothing
      fulfillmentState = Nothing
      fulfillmentCustomer = if isValueAddNP then tfCustomer res.phoneNumber else Nothing
      fulfillmentId = Just res.estimate.bppEstimateId.getId
      fulfillmentType = UCommonV2.tripCategoryToFulfillmentType <$> res.tripCategory
      fulfillmentStops = UCommon.mkStops' (Just startLoc) stops (Just endLoc)
      fulfillmentVehicle = tfVehicle res
   in Spec.Fulfillment
        { fulfillmentStops = fulfillmentStops,
          fulfillmentVehicle = Just fulfillmentVehicle,
          ..
        }

tfCustomer :: Maybe T.Text -> Maybe Spec.Customer
tfCustomer mbPhoneNumber = do
  let customerContact = Just $ Spec.Contact {contactPhone = mbPhoneNumber}
      customerPerson = Nothing
      returnData = Spec.Customer {customerContact = customerContact, customerPerson = customerPerson}
      allNothing = UCommonV2.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfVehicle :: DSelect.DSelectRes -> Spec.Vehicle
tfVehicle res =
  let (category, variant) = UCommon.castVehicleVariant res.variant
      vehicleColor = Nothing
      vehicleMake = Nothing
      vehicleModel = Nothing
      vehicleRegistration = Nothing
      vehicleVariant = Just variant
      vehicleCategory = Just category
      vehicleCapacity = Nothing
   in Spec.Vehicle {..}

tfOrderItem :: DSelect.DSelectRes -> Bool -> Spec.Item
tfOrderItem res isValueAddNP =
  let itemDescriptor = Nothing
      itemFulfillmentIds = Nothing
      itemLocationIds = Nothing
      itemPaymentIds = Nothing
      itemId = Just res.estimate.itemId
      itemTags =
        if isValueAddNP
          then Just $ mkItemTags res
          else Nothing
      itemPrice = tfPrice res
   in Spec.Item
        { itemPrice = Just itemPrice,
          ..
        }

mkItemTags :: DSelect.DSelectRes -> [Spec.TagGroup]
mkItemTags res =
  let itemTags = [mkAutoAssignEnabledTagGroup res] <> mkSelectResDetailsTagGroup res
      itemTags' = if isJust res.customerExtraFee then mkCustomerTipTagGroup res : itemTags else itemTags
      itemTags'' = if not (null res.remainingEstimateBppIds) then mkOtheEstimatesTagGroup res : itemTags' else itemTags'
      itemTags''' = [mkAdvancedBookingEnabledTagGroup res, mkDeviceIdTagGroup res, mkDisabilityDisableTagGroup res, mkSafetyPlusTagGroup res, mkPetRideTagGroup res, mkBillingCategoryTagGroup res] <> itemTags''
   in itemTags'''

mkCustomerTipTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkCustomerTipTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.CUSTOMER_TIP_INFO,
              descriptorName = Just "Customer Tip Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = (\_ -> Just $ show Tags.CUSTOMER_TIP) =<< res.customerExtraFee,
                        descriptorName = (\_ -> Just "Customer Tip") =<< res.customerExtraFee,
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = (\charges -> Just $ show charges.getMoney) =<< res.customerExtraFee
              }
          ]
    }

mkOtheEstimatesTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkOtheEstimatesTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.ESTIMATIONS,
              descriptorName = Just "Customer Tip Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.OTHER_SELECT_ESTIMATES,
                        descriptorName = Just "Other selected estimates for book any",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show (getId <$> res.remainingEstimateBppIds)
              }
          ]
    }

mkAutoAssignEnabledTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkAutoAssignEnabledTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.AUTO_ASSIGN_ENABLED,
              descriptorName = Just "Auto Assign Enabled",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.IS_AUTO_ASSIGN_ENABLED,
                        descriptorName = Just "Auto Assign Enabled",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show res.autoAssignEnabled
              }
          ]
    }

mkDeviceIdTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkDeviceIdTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.DEVICE_ID_INFO,
              descriptorName = Just "Device Id Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.DEVICE_ID_FLAG,
                        descriptorName = Just "Multiple or No Device Id Exists",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = show <$> res.isMultipleOrNoDeviceIdExist
              },
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.TO_UPDATE_DEVICE_ID,
                        descriptorName = Just "Is Device Id Update Required",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show res.toUpdateDeviceIdInfo
              }
          ]
    }

mkAdvancedBookingEnabledTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
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
                tagValue = Just $ show res.isAdvancedBookingEnabled
              }
          ]
    }

mkDisabilityDisableTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkDisabilityDisableTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.CUSTOMER_INFO,
              descriptorName = Just "Disability Disable Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.CUSTOMER_DISABILITY_DISABLE,
                        descriptorName = Just "Disability Disable Flag",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = (Just . T.pack . show) =<< res.disabilityDisable
              }
          ]
    }

mkPetRideTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkPetRideTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.PET_ORDER_INFO,
              descriptorName = Just "Pet Order Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.IS_PET_RIDE,
                        descriptorName = Just "Is Pet Ride",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = (Just . T.pack . show) =<< res.isPetRide
              }
          ]
    }

mkBillingCategoryTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkBillingCategoryTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.BILLING_CATEGORY_INFO,
              descriptorName = Just "Billing Category Info",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.BILLING_CATEGORY,
                        descriptorName = Just "Billing Category",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show res.billingCategory
              }
          ]
    }

mkSelectResDetailsTagGroup :: DSelect.DSelectRes -> [Spec.TagGroup]
mkSelectResDetailsTagGroup res =
  maybe
    []
    ( \case
        (DSelect.DSelectResDelivery details) -> [getDeliveryTags details]
    )
    res.selectResDetails
  where
    getDeliveryTags details =
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
                          { descriptorCode = Just $ show Tags.PARCEL_TYPE,
                            descriptorName = Just "Delivery Parcel Type",
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = Just False,
                    tagValue = Just $ show details.parcelType
                  },
                Spec.Tag
                  { tagDescriptor =
                      Just $
                        Spec.Descriptor
                          { descriptorCode = Just $ show Tags.PARCEL_QUANTITY,
                            descriptorName = Just "Delivery Parcel Quantity",
                            descriptorShortDesc = Nothing
                          },
                    tagDisplay = Just False,
                    tagValue = show <$> details.quantity
                  }
              ]
        }

mkSafetyPlusTagGroup :: DSelect.DSelectRes -> Spec.TagGroup
mkSafetyPlusTagGroup res =
  Spec.TagGroup
    { tagGroupDisplay = Just False,
      tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just $ show Tags.SAFETY_PLUS_INFO,
              descriptorName = Just "Safety plus data",
              descriptorShortDesc = Nothing
            },
      tagGroupList =
        Just
          [ Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just $ show Tags.PREFER_SAFETY_PLUS,
                        descriptorName = Just "Safety Plus Enabled",
                        descriptorShortDesc = Nothing
                      },
                tagDisplay = Just False,
                tagValue = Just $ show res.preferSafetyPlus
              }
          ]
    }

tfPrice :: DSelect.DSelectRes -> Spec.Price
tfPrice res =
  let priceCurrency = Just $ show res.estimate.estimatedFare.currency
      priceValue = Just $ show res.estimate.estimatedFare.amount
      priceComputedValue = Nothing
      priceMaximumValue = Nothing
      priceMinimumValue = Nothing
      priceOfferedValue = Nothing
   in Spec.Price {..}

tfPayments :: DSelect.DSelectRes -> BecknConfig -> RiderConfig -> Maybe [Spec.Payment]
tfPayments res bapConfig riderConfig = do
  let mPrice = Just res.estimate.estimatedFare
  let mkParams = SLMPM.mkBknPaymentParams res.paymentMethodInfo bapConfig riderConfig
  Just $ L.singleton $ mkPayment (show res.city) (show bapConfig.collectedBy) Enums.NOT_PAID mPrice Nothing mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee False res.paymentMode

tfProvider :: DSelect.DSelectRes -> Spec.Provider
tfProvider res =
  let providerDescriptor = Nothing
      providerFulfillments = Nothing
      providerItems = Nothing
      providerLocations = Nothing
      providerPayments = Nothing
      providerId = Just res.providerId
   in Spec.Provider {..}
