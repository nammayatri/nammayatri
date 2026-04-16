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
    mkOnSelectMessageForQuoteV2,
    DOnSelectReq (..),
    TransporterInfo (..),
  )
where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as UtilsV2
import BecknV2.OnDemand.Utils.Constructors
import BecknV2.OnDemand.Utils.Payment
import BecknV2.Utils
import qualified Data.Text as T
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Quote as DQuoteStatic
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as Variant
import Kernel.Prelude
import qualified Kernel.Types.Common as Common (mkPrice)
import Kernel.Types.Id (ShortId)
import qualified Kernel.Types.Price
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
mkOnSelectMessageV2 isValueAddNP bppConfig _merchant mbFarePolicy req@DOnSelectReq {..} = do
  let fulfillments = [mkFulfillmentV2 req driverQuote isValueAddNP]
  Spec.OnSelectReqMessage $
    Just
      emptyOrder
        { Spec.orderFulfillments = Just fulfillments,
          Spec.orderItems = Just $ map (\fulf -> mkItemV2 fulf vehicleServiceTierItem driverQuote mbFarePolicy) fulfillments,
          Spec.orderQuote = Just $ mkQuoteV2 driverQuote req.now,
          Spec.orderPayments = Nothing,
          Spec.orderProvider = mkProvider bppConfig,
          Spec.orderCancellationTerms = Just $ Utils.tfCancellationTerms Nothing (Just Enums.RIDE_ASSIGNED)
        }

-- | Build on_select message for quote-based (rental/special-zone) select
mkOnSelectMessageForQuoteV2 ::
  DBC.BecknConfig ->
  DM.Merchant ->
  SearchRequest ->
  DQuoteStatic.Quote ->
  UTCTime ->
  Spec.OnSelectReqMessage
mkOnSelectMessageForQuoteV2 bppConfig merchant searchRequest quote now = do
  let fulfillment = mkQuoteFulfillmentV2 searchRequest quote
      paymentPrice = Just $ Common.mkPrice (Just quote.currency) quote.estimatedFare
      mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bppConfig.paymentParamsJson
      payment = mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID paymentPrice Nothing mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing
      nominalDiff = diffUTCTime quote.validTill now
      roundedFare = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just quote.currency) quote.estimatedFare
      quotation =
        Spec.Quotation
          { quotationBreakup = Just $ mkQuoteStaticBreakup quote,
            quotationPrice =
              Just
                emptyPrice
                  { Spec.priceCurrency = Just $ show quote.currency,
                    Spec.priceValue = Just roundedFare
                  },
            quotationTtl = Just $ formatTimeDifference nominalDiff
          }
      item =
        emptyItem
          { Spec.itemId = Just quote.id.getId,
            Spec.itemFulfillmentIds = Just [quote.id.getId],
            Spec.itemPrice =
              Just
                emptyPrice
                  { Spec.priceCurrency = Just $ show quote.currency,
                    Spec.priceValue = Just roundedFare
                  },
            Spec.itemDescriptor =
              Just
                Spec.Descriptor
                  { descriptorCode = Just $ tripCategoryToItemDescriptorCode quote.tripCategory,
                    descriptorShortDesc = quote.vehicleServiceTierName,
                    descriptorName = quote.vehicleServiceTierName
                  },
            Spec.itemCategoryIds = Just [Utils.tripCategoryToCategoryCode quote.tripCategory]
          }
  Spec.OnSelectReqMessage $
    Just
      emptyOrder
        { Spec.orderFulfillments = Just [fulfillment],
          Spec.orderItems = Just [item],
          Spec.orderQuote = Just quotation,
          Spec.orderPayments = Just [payment],
          Spec.orderProvider = mkProvider bppConfig,
          Spec.orderCancellationTerms = Just $ Utils.tfCancellationTerms Nothing (Just Enums.RIDE_CONFIRMED)
        }
  where
    mkQuoteFulfillmentV2 sr q =
      let vehicleVariant = Variant.castServiceTierToVariant q.vehicleServiceTier
          (cat, var) = Utils.castVariant vehicleVariant
       in emptyFulfillment
            { Spec.fulfillmentId = Just q.id.getId,
              Spec.fulfillmentStops = Utils.mkStops' sr.fromLocation sr.toLocation sr.stops Nothing,
              Spec.fulfillmentVehicle = Just emptyVehicle {Spec.vehicleCategory = Just cat, Spec.vehicleVariant = Just var},
              Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType q.tripCategory
            }
    mkQuoteStaticBreakup q =
      let fp = mkFareParamsBreakups (\x -> x) (\title money -> (title, money)) q.fareParams
          normalized = mapMaybe (\(title, money) -> (,money) <$> Utils.normalizeBreakupTitle title) fp
       in Utils.aggregateBreakupsWithTotal (Just q.estimatedFare) q.currency normalized

mkFulfillmentV2 :: DOnSelectReq -> DQuote.DriverQuote -> Bool -> Spec.Fulfillment
mkFulfillmentV2 dReq quote _isValueAddNP = do
  emptyFulfillment
    { Spec.fulfillmentId = Just quote.estimateId.getId,
      Spec.fulfillmentStops = Utils.mkStops' dReq.searchRequest.fromLocation dReq.searchRequest.toLocation dReq.searchRequest.stops Nothing,
      Spec.fulfillmentVehicle = Just $ mkVehicleV2 quote,
      Spec.fulfillmentType = Just $ UtilsV2.tripCategoryToFulfillmentType quote.tripCategory,
      Spec.fulfillmentAgent = Nothing,
      Spec.fulfillmentTags = Utils.mkRouteInfoTagsFromPolyline dReq.searchRequest.encodedPolyline
    }

_mkPaymentV2 :: DBC.BecknConfig -> DM.Merchant -> DQuote.DriverQuote -> Maybe Text -> Spec.Payment
_mkPaymentV2 bppConfig merchant driverQuote mbPaymentId = do
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

_mkAgentV2 :: DQuote.DriverQuote -> Bool -> Spec.Agent
_mkAgentV2 quote isValueAddNP =
  emptyAgent
    { Spec.agentPerson = Just $ _mkAgentPersonV2 quote isValueAddNP
    }

_mkAgentPersonV2 :: DQuote.DriverQuote -> Bool -> Spec.Person
_mkAgentPersonV2 quote isValueAddNP =
  emptyPerson
    { Spec.personName = Just quote.driverName,
      Spec.personTags = if isValueAddNP then _mkAgentTagsV2 quote else Nothing
    }

_mkAgentTagsV2 :: DQuote.DriverQuote -> Maybe [Spec.TagGroup]
_mkAgentTagsV2 quote =
  Tags.buildTagGroups
    [ Tags.RATING Tags.~=? (show . (.getCenti) <$> quote.driverRating)
    ]

mkItemV2 :: Spec.Fulfillment -> DVST.VehicleServiceTier -> DQuote.DriverQuote -> Maybe FarePolicyD.FullFarePolicy -> Spec.Item
mkItemV2 fulfillment vehicleServiceTierItem quote mbFarePolicy = do
  let fulfillmentId = fulfillment.fulfillmentId & fromMaybe (error $ "It should never happen as we have created fulfillment:-" <> show fulfillment)
  emptyItem
    { Spec.itemId = Just $ "I-" <> quote.estimateId.getId,
      Spec.itemFulfillmentIds = Just [fulfillmentId],
      Spec.itemPrice = Just $ mkPriceV2 quote,
      Spec.itemTags = mkItemTagsV2 quote mbFarePolicy,
      Spec.itemDescriptor = mkItemDescriptor vehicleServiceTierItem quote,
      Spec.itemCategoryIds = Just [Utils.tripCategoryToCategoryCode quote.tripCategory]
    }

mkItemDescriptor :: DVST.VehicleServiceTier -> DQuote.DriverQuote -> Maybe Spec.Descriptor
mkItemDescriptor vehicleServiceTierItem quote =
  Just
    Spec.Descriptor
      { descriptorCode = Just $ tripCategoryToItemDescriptorCode quote.tripCategory,
        descriptorShortDesc = vehicleServiceTierItem.shortDescription,
        descriptorName = Just vehicleServiceTierItem.name
      }

-- | Map trip category to spec-compliant item descriptor code (RIDE or RENTAL)
tripCategoryToItemDescriptorCode :: TripCategory -> Text
tripCategoryToItemDescriptorCode (Rental _) = "RENTAL"
tripCategoryToItemDescriptorCode _ = "RIDE"

mkPriceV2 :: DQuote.DriverQuote -> Spec.Price
mkPriceV2 quote =
  let roundedFare = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just quote.currency) quote.estimatedFare
   in emptyPrice
        { Spec.priceCurrency = Just $ show quote.currency,
          Spec.priceValue = Just roundedFare
        }

mkItemTagsV2 :: DQuote.DriverQuote -> Maybe FarePolicyD.FullFarePolicy -> Maybe [Spec.TagGroup]
mkItemTagsV2 quote mbFarePolicy = do
  let farePolicyTags = mkSpecFarePolicyTags mbFarePolicy
      infoTags = mkSpecInfoTags quote.distanceToPickup quote.durationToPickup
      featureList = Utils.mkFeatureListTags Nothing
  Just $ catMaybes [farePolicyTags, infoTags] <> [featureList]

-- | Build FARE_POLICY tag group with only spec-required tags
mkSpecFarePolicyTags :: Maybe FarePolicyD.FullFarePolicy -> Maybe Spec.TagGroup
mkSpecFarePolicyTags mbFullFP =
  Utils.mkSpecFarePolicyTagsFromPolicy (FarePolicyD.fullFarePolicyToFarePolicy <$> mbFullFP)

-- | Build INFO tag group with spec-required tags
mkSpecInfoTags :: Meters -> Seconds -> Maybe Spec.TagGroup
mkSpecInfoTags distanceToPickup durationToPickup =
  Just $
    Tags.getFullTagGroup
      Tags.GENERAL_INFO
      [ Tags.getFullTag Tags.DISTANCE_TO_NEAREST_DRIVER_METER (Just $ show distanceToPickup.getMeters),
        Tags.getFullTag Tags.ETA_TO_NEAREST_DRIVER_MIN (Just $ show (durationToPickup.getSeconds `div` 60))
      ]

mkQuoteV2 :: DQuote.DriverQuote -> UTCTime -> Spec.Quotation
mkQuoteV2 quote now = do
  let nominalDifferenceTime = diffUTCTime quote.validTill now
  Spec.Quotation
    { quotationBreakup = Just $ mkQuoteBreakupInner quote,
      quotationPrice = mkQuotationPrice quote,
      quotationTtl = Just $ formatTimeDifference nominalDifferenceTime
    }

mkQuoteBreakupInner :: DQuote.DriverQuote -> [Spec.QuotationBreakupInner]
mkQuoteBreakupInner driverQuote =
  let fareParams = mkFareParamsBreakups (\x -> x) (\title money -> (title, money)) driverQuote.fareParams
      normalized = mapMaybe (\(title, money) -> (,money) <$> Utils.normalizeBreakupTitle title) fareParams
   in Utils.aggregateBreakupsWithTotal (Just driverQuote.estimatedFare) driverQuote.currency normalized

mkQuotationPrice :: DQuote.DriverQuote -> Maybe Spec.Price
mkQuotationPrice quote =
  let roundedFare = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just quote.currency) quote.estimatedFare
   in Just
        emptyPrice
          { Spec.priceCurrency = Just $ show quote.currency,
            Spec.priceOfferedValue = Nothing,
            Spec.priceValue = Just roundedFare
          }

mkProvider :: DBC.BecknConfig -> Maybe Spec.Provider
mkProvider becknConfig = do
  return $ emptyProvider {Spec.providerId = Just $ becknConfig.subscriberId}
