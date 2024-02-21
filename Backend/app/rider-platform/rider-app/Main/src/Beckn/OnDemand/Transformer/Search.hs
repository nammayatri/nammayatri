{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified BecknV2.OnDemand.Enums as Enums
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

buildBecknSearchReqV2 :: (Kernel.Types.App.MonadFlow m) => Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Domain.Action.UI.Search.SearchReqLocation -> [Domain.Action.UI.Search.SearchReqLocation] -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Domain.Types.Merchant.Merchant -> Kernel.Prelude.BaseUrl -> Kernel.Types.Beckn.Context.City -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> Data.Time.UTCTime -> Maybe [Maps.RouteInfo] -> BecknConfig -> m BecknV2.OnDemand.Types.SearchReq
buildBecknSearchReqV2 action domain origin stops searchId distance duration customerLanguage disabilityTag merchant bapUri city mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes bapConfig = do
  messageId <- generateGUIDText
  searchReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain messageId (Just searchId.getId) merchant.bapId bapUri Nothing Nothing city merchant.country
  let searchReqMessage_ = buildSearchMessageV2 origin stops distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes merchant bapConfig
  pure $ BecknV2.OnDemand.Types.SearchReq {searchReqContext = searchReqContext_, searchReqMessage = searchReqMessage_}

buildSearchMessageV2 ::
  Domain.Action.UI.Search.SearchReqLocation ->
  [Domain.Action.UI.Search.SearchReqLocation] ->
  Maybe Kernel.Types.Common.Meters ->
  Maybe Kernel.Types.Common.Seconds ->
  Maybe Tools.Maps.Language ->
  Maybe Data.Text.Text ->
  Maybe [Tools.Maps.LatLong] ->
  Maybe Data.Text.Text ->
  Maybe Kernel.Prelude.Bool ->
  Data.Time.UTCTime ->
  Maybe [Maps.RouteInfo] ->
  Domain.Types.Merchant.Merchant ->
  BecknConfig ->
  BecknV2.OnDemand.Types.SearchReqMessage
buildSearchMessageV2 origin stops distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes merchant bapConfig = do
  let searchReqMessageIntent_ = tfIntent origin stops customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes merchant bapConfig
  BecknV2.OnDemand.Types.SearchReqMessage {searchReqMessageIntent = searchReqMessageIntent_}

tfCustomer :: Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> Maybe BecknV2.OnDemand.Types.Customer
tfCustomer customerLanguage disabilityTag mbPhoneNumber = do
  let customerContact_ = Nothing
      customerPerson_ = tfPerson customerLanguage disabilityTag mbPhoneNumber
      returnData = BecknV2.OnDemand.Types.Customer {customerContact = customerContact_, customerPerson = customerPerson_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfFulfillment :: Domain.Action.UI.Search.SearchReqLocation -> [Domain.Action.UI.Search.SearchReqLocation] -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> Data.Time.UTCTime -> Maybe [Maps.RouteInfo] -> Maybe BecknV2.OnDemand.Types.Fulfillment
tfFulfillment origin stops customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes = do
  let fulfillmentAgent_ = Nothing
      fulfillmentId_ = Nothing
      fulfillmentState_ = Nothing
      fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops origin stops startTime
      fulfillmentTags_ = Beckn.OnDemand.Utils.Search.mkSearchFulFillmentTags distance duration mbPoints isReallocationEnabled multipleRoutes
      fulfillmentType_ = Nothing
      fulfillmentVehicle_ = Nothing
      fulfillmentCustomer_ = tfCustomer customerLanguage disabilityTag mbPhoneNumber
      returnData = BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfIntent ::
  Domain.Action.UI.Search.SearchReqLocation ->
  [Domain.Action.UI.Search.SearchReqLocation] ->
  Maybe Tools.Maps.Language ->
  Maybe Data.Text.Text ->
  Maybe Kernel.Types.Common.Meters ->
  Maybe Kernel.Types.Common.Seconds ->
  Maybe [Tools.Maps.LatLong] ->
  Maybe Data.Text.Text ->
  Maybe Kernel.Prelude.Bool ->
  Data.Time.UTCTime ->
  Maybe [Maps.RouteInfo] ->
  Domain.Types.Merchant.Merchant ->
  BecknConfig ->
  Maybe BecknV2.OnDemand.Types.Intent
tfIntent origin stops customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes merchant bapConfig = do
  let intentTags_ = Nothing
      intentFulfillment_ = tfFulfillment origin stops customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled startTime multipleRoutes
      intentPayment_ = tfPayment merchant bapConfig
      returnData = BecknV2.OnDemand.Types.Intent {intentFulfillment = intentFulfillment_, intentPayment = intentPayment_, intentTags = intentTags_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData

tfPayment :: Domain.Types.Merchant.Merchant -> BecknConfig -> Maybe Spec.Payment
tfPayment merchant bapConfig = do
  let mkParams :: (Maybe BknPaymentParams) = (readMaybe . T.unpack) =<< bapConfig.paymentParamsJson
  Just $ mkPayment (show merchant.defaultCity) (show bapConfig.collectedBy) Enums.NOT_PAID Nothing Nothing mkParams bapConfig.settlementType bapConfig.settlementWindow bapConfig.staticTermsUrl bapConfig.buyerFinderFee

tfPerson :: Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> Maybe BecknV2.OnDemand.Types.Person
tfPerson customerLanguage disabilityTag mbPhoneNumber = do
  let personId_ = Nothing
      personImage_ = Nothing
      personName_ = Nothing
      personTags_ = Beckn.OnDemand.Utils.Search.mkCustomerInfoTags customerLanguage disabilityTag mbPhoneNumber
      returnData = BecknV2.OnDemand.Types.Person {personId = personId_, personImage = personImage_, personName = personName_, personTags = personTags_}
      allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then Nothing
    else Just returnData
