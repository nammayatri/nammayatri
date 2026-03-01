module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import BecknV2.OnDemand.Utils.Payment
import Data.Text as T
import Domain.Types
import Domain.Types.BecknConfig
import qualified Domain.Types.RiderPreferredOption as DRPO
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import SharedLogic.Search as SLS

buildBecknSearchReqV2 :: SLS.SearchRes -> BecknConfig -> Kernel.Prelude.BaseUrl -> Text -> Either Text BecknV2.OnDemand.Types.SearchReq
buildBecknSearchReqV2 res@SLS.SearchRes {..} bapConfig bapUri messageId = do
  ttl <- maybe (Left "Invalid ttl") (Right . BecknV2.OnDemand.Utils.Common.computeTtlISO8601) bapConfig.searchTTLSec
  let searchReqContext_ = BecknV2.OnDemand.Utils.Context.buildContextV2' now Kernel.Types.Beckn.Context.SEARCH Kernel.Types.Beckn.Context.MOBILITY messageId (Just searchRequest.id.getId) merchant.bapId bapUri Nothing Nothing city merchant.country (Just ttl)
      searchReqMessage_ = buildSearchMessageV2 res bapConfig
  pure $ BecknV2.OnDemand.Types.SearchReq {searchReqContext = searchReqContext_, searchReqMessage = searchReqMessage_}

buildSearchMessageV2 :: SLS.SearchRes -> BecknConfig -> BecknV2.OnDemand.Types.SearchReqMessage
buildSearchMessageV2 res bapConfig = BecknV2.OnDemand.Types.SearchReqMessage {searchReqMessageIntent = tfIntent res bapConfig}

tfCustomer :: Maybe Tags.Taggings -> Maybe BecknV2.OnDemand.Types.Customer
tfCustomer taggings = do
  let customerContact_ = Nothing
      customerPerson_ = tfPerson taggings
      returnData = BecknV2.OnDemand.Types.Customer {customerContact = customerContact_, customerPerson = customerPerson_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfFulfillment :: SLS.SearchRes -> Maybe BecknV2.OnDemand.Types.Fulfillment
tfFulfillment SLS.SearchRes {..} = do
  let fulfillmentAgent_ = Nothing
      fulfillmentId_ = Nothing
      fulfillmentState_ = Nothing
      -- ONDC v2.1.0: For scheduled rides (startTime > now), include pickup time window duration
      isScheduled = startTime > now
      mbScheduledPickupDuration = Beckn.OnDemand.Utils.Common.mkScheduledPickupDuration isScheduled
      fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops origin stops startTime mbScheduledPickupDuration
      fulfillmentTags_ = Tags.convertToTagGroup . (.fulfillmentTags) =<< taggings
      fulfillmentType_ = Nothing
      fulfillmentVehicle_ = Nothing
      fulfillmentCustomer_ = tfCustomer taggings
      returnData = BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfIntent :: SLS.SearchRes -> BecknConfig -> Maybe BecknV2.OnDemand.Types.Intent
tfIntent res bapConfig = do
  let intentTags_ = Nothing
      intentFulfillment_ = tfFulfillment res
      intentPayment_ = tfPayment res bapConfig
      intentCategory_ = riderPreferredOptionToCategory res.riderPreferredOption
      returnData = BecknV2.OnDemand.Types.Intent {intentCategory = intentCategory_, intentFulfillment = intentFulfillment_, intentPayment = intentPayment_, intentTags = intentTags_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfPayment :: SLS.SearchRes -> BecknConfig -> Maybe Spec.Payment
tfPayment res bapConfig = do
  let updatedPaymentTags =
        maybe
          []
          ( \tag ->
              [ (Tags.SETTLEMENT_WINDOW, Just $ fromMaybe "PT1D" bapConfig.settlementWindow),
                (Tags.BUYER_FINDER_FEES_PERCENTAGE, Just $ fromMaybe "0" bapConfig.buyerFinderFee),
                (Tags.STATIC_TERMS, Just $ maybe "https://api.example-bap.com/booking/terms" Kernel.Prelude.showBaseUrl bapConfig.staticTermsUrl),
                (Tags.SETTLEMENT_TYPE, bapConfig.settlementType)
              ]
                <> (tag.paymentTags)
          )
          (res.taggings)
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bapConfig.paymentParamsJson
  Just $ mkPayment' updatedPaymentTags (show bapConfig.collectedBy) Enums.NOT_PAID Nothing Nothing mkParams

tfPerson :: Maybe Tags.Taggings -> Maybe BecknV2.OnDemand.Types.Person
tfPerson taggings = do
  let personId_ = Nothing
      personImage_ = Nothing
      personName_ = Nothing
      basePersonTags = Tags.convertToTagGroup . (.personTags) =<< taggings
      -- ONDC v2.1.0: build per-type disability tag groups alongside backward-compat CUSTOMER_DISABILITY tag
      disabilityTag = do
        t <- taggings
        mbVal <- Kernel.Prelude.lookup Tags.CUSTOMER_DISABILITY (Tags.personTags t)
        mbVal
      disabilityTagGroups = Tags.mkDisabilityTagGroups disabilityTag
      personTags_ = case (basePersonTags, disabilityTagGroups) of
        (Just base, _ : _) -> Just (base <> disabilityTagGroups)
        (Nothing, _ : _) -> Just disabilityTagGroups
        _ -> basePersonTags
      returnData = BecknV2.OnDemand.Types.Person {personGender = Nothing, personId = personId_, personImage = personImage_, personName = personName_, personTags = personTags_} -- TODO: ONDC v2.1.0 - populate rider gender in search request when available
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

riderPreferredOptionToCategory :: DRPO.RiderPreferredOption -> Maybe BecknV2.OnDemand.Types.Category
riderPreferredOptionToCategory rpo = do
  let code = case rpo of
        DRPO.OneWay -> "ON_DEMAND_TRIP"
        DRPO.Rental -> "ON_DEMAND_RENTAL"
        DRPO.InterCity -> "INTERCITY_TRIP"
        DRPO.Ambulance -> "ON_DEMAND_TRIP"
        DRPO.Delivery -> "ON_DEMAND_TRIP"
        DRPO.PublicTransport -> "ON_DEMAND_TRIP"
        DRPO.FixedRoute -> "ON_DEMAND_TRIP"
  Just $
    BecknV2.OnDemand.Types.Category
      { categoryDescriptor =
          Just $
            BecknV2.OnDemand.Types.Descriptor
              { descriptorCode = Just code,
                descriptorName = Just code,
                descriptorShortDesc = Nothing
              },
        categoryId = Just code
      }
