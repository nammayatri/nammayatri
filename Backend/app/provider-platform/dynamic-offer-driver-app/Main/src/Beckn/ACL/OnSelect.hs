{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnSelect
  ( mkOnSelectMessageV2,
    DOnSelectReq (..),
    TransporterInfo (..),
  )
where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import BecknV2.OnDemand.Utils.Payment
import BecknV2.Utils
import qualified Data.Text as T
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant as DM
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.VehicleServiceTier as DVST
import Kernel.Prelude
import qualified Kernel.Types.Common as Common (mkPrice)
import Kernel.Types.Id (ShortId)
import Kernel.Utils.Common
import SharedLogic.FareCalculator (mkFareParamsBreakups)

data DOnSelectReq = DOnSelectReq
  { transporterInfo :: TransporterInfo,
    vehicleServiceTierItem :: DVST.VehicleServiceTier,
    searchRequest :: SearchRequest,
    driverQuote :: DQuote.DriverQuote,
    taggings :: Tags.Taggings,
    now :: UTCTime
  }

data TransporterInfo = TransporterInfo
  { merchantShortId :: ShortId DM.Merchant,
    name :: Text,
    contacts :: Text,
    ridesInProgress :: Int,
    ridesCompleted :: Int,
    ridesConfirmed :: Int
  }

mkOnSelectMessageV2 ::
  Bool ->
  DBC.BecknConfig ->
  DM.Merchant ->
  Maybe FarePolicyD.FullFarePolicy ->
  DOnSelectReq ->
  Spec.OnSelectReqMessage
mkOnSelectMessageV2 isValueAddNP bppConfig merchant mbFarePolicy req@DOnSelectReq {..} = do
  let fulfillments = [mkFulfillmentV2 req driverQuote isValueAddNP]
  let paymentV2 = mkPaymentV2 bppConfig merchant driverQuote Nothing
  Spec.OnSelectReqMessage $
    Just
      emptyOrder
        { Spec.orderFulfillments = Just fulfillments,
          Spec.orderItems = Just $ map (\fulf -> mkItemV2 fulf vehicleServiceTierItem driverQuote mbFarePolicy taggings) fulfillments,
          Spec.orderQuote = Just $ mkQuoteV2 driverQuote req.now,
          Spec.orderPayments = Just [paymentV2],
          Spec.orderProvider = mkProvider bppConfig
        }

mkFulfillmentV2 :: DOnSelectReq -> DQuote.DriverQuote -> Bool -> Spec.Fulfillment
mkFulfillmentV2 dReq quote isValueAddNP = do
  emptyFulfillment
    { Spec.fulfillmentId = Just quote.id.getId,
      Spec.fulfillmentStops = Utils.mkStops' dReq.searchRequest.fromLocation dReq.searchRequest.toLocation dReq.searchRequest.stops Nothing,
      Spec.fulfillmentVehicle = Just $ mkVehicleV2 quote,
      Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType quote.tripCategory,
      Spec.fulfillmentAgent = Just $ mkAgentV2 quote isValueAddNP
    }

mkPaymentV2 :: DBC.BecknConfig -> DM.Merchant -> DQuote.DriverQuote -> Maybe Text -> Spec.Payment
mkPaymentV2 bppConfig merchant driverQuote mbPaymentId = do
  let mPrice = Just $ Common.mkPrice (Just driverQuote.currency) driverQuote.estimatedFare
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
  mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice mbPaymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

mkVehicleV2 :: DQuote.DriverQuote -> Spec.Vehicle
mkVehicleV2 quote =
  let (category, variant) = Utils.castVariant quote.vehicleVariant
   in emptyVehicle
        { Spec.vehicleCategory = Just category,
          Spec.vehicleVariant = Just variant
        }

mkAgentV2 :: DQuote.DriverQuote -> Bool -> Spec.Agent
mkAgentV2 quote isValueAddNP =
  emptyAgent
    { Spec.agentPerson = Just $ mkAgentPersonV2 quote isValueAddNP
    }

mkAgentPersonV2 :: DQuote.DriverQuote -> Bool -> Spec.Person
mkAgentPersonV2 quote isValueAddNP =
  emptyPerson
    { Spec.personName = Just quote.driverName,
      Spec.personTags = if isValueAddNP then mkAgentTagsV2 quote else Nothing
    }

mkAgentTagsV2 :: DQuote.DriverQuote -> Maybe [Spec.TagGroup]
mkAgentTagsV2 quote =
  Tags.buildTagGroups
    [ Tags.RATING Tags.~=? (show . (.getCenti) <$> quote.driverRating)
    ]

mkItemV2 :: Spec.Fulfillment -> DVST.VehicleServiceTier -> DQuote.DriverQuote -> Maybe FarePolicyD.FullFarePolicy -> Tags.Taggings -> Spec.Item
mkItemV2 fulfillment vehicleServiceTierItem quote mbFarePolicy taggings = do
  let fulfillmentId = fulfillment.fulfillmentId & fromMaybe (error $ "It should never happen as we have created fulfillment:-" <> show fulfillment)
  emptyItem
    { Spec.itemId = Just quote.estimateId.getId,
      Spec.itemFulfillmentIds = Just [fulfillmentId],
      Spec.itemPrice = Just $ mkPriceV2 quote,
      Spec.itemTags = mkItemTagsV2 quote.estimatedFare quote.fareParams.customerCancellationDues quote.fareParams.congestionChargeViaDp mbFarePolicy taggings,
      Spec.itemDescriptor = mkItemDescriptor vehicleServiceTierItem,
      Spec.itemCategoryIds = Just [Utils.tripCategoryToCategoryCode quote.tripCategory]
    }

mkItemDescriptor :: DVST.VehicleServiceTier -> Maybe Spec.Descriptor
mkItemDescriptor vehicleServiceTierItem =
  Just
    Spec.Descriptor
      { descriptorCode = Just $ show vehicleServiceTierItem.serviceTierType,
        descriptorShortDesc = vehicleServiceTierItem.shortDescription,
        descriptorName = Just vehicleServiceTierItem.name
      }

mkPriceV2 :: DQuote.DriverQuote -> Spec.Price
mkPriceV2 quote =
  emptyPrice
    { Spec.priceCurrency = Just $ show quote.currency,
      Spec.priceValue = Just $ show $ quote.estimatedFare
    }

mkItemTagsV2 :: HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe FarePolicyD.FullFarePolicy -> Tags.Taggings -> Maybe [Spec.TagGroup]
mkItemTagsV2 estimatedFare mbCancellationCharge congestionChargeViaDp mbFarePolicy taggings = do
  let farePolicyTag = Utils.mkRateCardTag Nothing mbCancellationCharge Nothing estimatedFare congestionChargeViaDp (Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy) Nothing Nothing Nothing
  Tags.convertToTagGroup taggings.itemTags <> farePolicyTag

mkQuoteV2 :: DQuote.DriverQuote -> UTCTime -> Spec.Quotation
mkQuoteV2 quote now = do
  let nominalDifferenceTime = diffUTCTime quote.validTill now
  Spec.Quotation
    { quotationBreakup = Just $ mkQuoteBreakupInner quote,
      quotationPrice = mkQuotationPrice quote,
      quotationTtl = Just $ formatTimeDifference nominalDifferenceTime
    }

mkQuoteBreakupInner :: DQuote.DriverQuote -> [Spec.QuotationBreakupInner]
mkQuoteBreakupInner quote = do
  let fareParams = mkFareParamsBreakups mkBreakupPrice mkQuotationBreakupInner quote.fareParams
   in filter filterRequiredBreakups fareParams
  where
    mkBreakupPrice money =
      Just
        emptyPrice
          { Spec.priceCurrency = Just $ show quote.currency,
            Spec.priceValue = Just $ encodeToText money
          }
    mkQuotationBreakupInner title price =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerPrice = price,
          quotationBreakupInnerTitle = Just title
        }
    filterRequiredBreakups breakup =
      breakup.quotationBreakupInnerTitle == Just (show Enums.BASE_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.SERVICE_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DEAD_KILOMETER_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DISTANCE_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_SELECTED_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.CUSTOMER_SELECTED_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOTAL_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.WAITING_OR_PICKUP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.EXTRA_TIME_FARE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.PARKING_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.NIGHT_SHIFT_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.SAFETY_PLUS_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.RIDE_STOP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.PER_STOP_CHARGES)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.LUGGAGE_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.DRIVER_ALLOWANCE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.RETURN_FEE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.BOOTH_CHARGE)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.RIDE_VAT)
        || breakup.quotationBreakupInnerTitle == Just (show Enums.TOLL_VAT)

mkQuotationPrice :: DQuote.DriverQuote -> Maybe Spec.Price
mkQuotationPrice quote =
  Just
    emptyPrice
      { Spec.priceCurrency = Just $ show quote.currency,
        Spec.priceOfferedValue = Just $ encodeToText quote.estimatedFare,
        Spec.priceValue = Just $ encodeToText quote.estimatedFare
      }

mkProvider :: DBC.BecknConfig -> Maybe Spec.Provider
mkProvider becknConfig = do
  return $ emptyProvider { Spec.providerId = Just $ becknConfig.subscriberId }
