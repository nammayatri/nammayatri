{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import qualified Data.Text
import qualified Data.Time
import qualified Data.UUID
import qualified Domain.Action.UI.Search
import Domain.Types
import Domain.Types.BecknConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.SearchRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import Kernel.Utils.Common
import qualified Tools.Maps
import qualified Tools.Maps as Maps

buildBecknSearchReqV2 :: Domain.Action.UI.Search.SearchRes -> BecknConfig -> Kernel.Prelude.BaseUrl -> Text -> Either Text BecknV2.OnDemand.Types.SearchReq
buildBecknSearchReqV2 res@Domain.Action.UI.Search.SearchRes {..} bapConfig bapUri messageId = do
  ttl <- maybe (Left "Invalid ttl") (Right . BecknV2.OnDemand.Utils.Common.computeTtlISO8601) bapConfig.searchTTLSec
  let searchReqContext_ = BecknV2.OnDemand.Utils.Context.buildContextV2' now Kernel.Types.Beckn.Context.SEARCH Kernel.Types.Beckn.Context.MOBILITY messageId (Just searchId.getId) merchant.bapId bapUri Nothing Nothing city merchant.country (Just ttl)
      searchReqMessage_ = buildSearchMessageV2 res bapConfig
  pure $ BecknV2.OnDemand.Types.SearchReq {searchReqContext = searchReqContext_, searchReqMessage = searchReqMessage_}

buildSearchMessageV2 :: Domain.Action.UI.Search.SearchRes -> BecknConfig -> BecknV2.OnDemand.Types.SearchReqMessage
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

tfFulfillment :: Domain.Action.UI.Search.SearchRes -> Maybe BecknV2.OnDemand.Types.Fulfillment
tfFulfillment Domain.Action.UI.Search.SearchRes {..} = do
  let fulfillmentAgent_ = Nothing
      fulfillmentId_ = Nothing
      fulfillmentState_ = Nothing
      fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops origin stops startTime
      fulfillmentTags_ = Tags.convertToTagGroup . (.fulfillmentTags) =<< taggings
      fulfillmentType_ = Nothing
      fulfillmentVehicle_ = Nothing
      fulfillmentCustomer_ = tfCustomer taggings
      returnData = BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfIntent :: Domain.Action.UI.Search.SearchRes -> BecknConfig -> Maybe BecknV2.OnDemand.Types.Intent
tfIntent res bapConfig = do
  let intentTags_ = Nothing
      intentFulfillment_ = tfFulfillment res
      intentPayment_ = tfPayment res bapConfig
      returnData = BecknV2.OnDemand.Types.Intent {intentFulfillment = intentFulfillment_, intentPayment = intentPayment_, intentTags = intentTags_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfPayment :: Domain.Action.UI.Search.SearchRes -> BecknConfig -> Maybe Spec.Payment
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
      personGender_ = Nothing
      personTags_ = Tags.convertToTagGroup . (.personTags) =<< taggings
      returnData = BecknV2.OnDemand.Types.Person {personId = personId_, personImage = personImage_, personName = personName_, personGender = personGender_, personTags = personTags_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData
