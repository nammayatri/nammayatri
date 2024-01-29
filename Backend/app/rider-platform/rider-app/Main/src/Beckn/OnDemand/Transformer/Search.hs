{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified BecknV2.OnDemand.Utils.Context
import qualified Data.Text
import qualified Data.UUID
import qualified Domain.Action.UI.Search.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.SearchRequest
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import Kernel.Utils.Common (type (:::))
import qualified Tools.Maps

buildBecknSearchReqV2 :: (Kernel.Types.App.MonadFlow m) => Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Domain.Types.Merchant.Merchant -> Kernel.Prelude.BaseUrl -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m BecknV2.OnDemand.Types.SearchReq
buildBecknSearchReqV2 action domain origin destination searchId distance duration customerLanguage disabilityTag merchant bapUri mbPoints mbPhoneNumber isReallocationEnabled = do
  searchReqContext_ <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain searchId.getId (Just searchId.getId) merchant.bapId bapUri Nothing Nothing merchant.defaultCity merchant.country
  searchReqMessage_ <- buildSearchMessageV2 origin destination distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber isReallocationEnabled
  pure $ BecknV2.OnDemand.Types.SearchReq {searchReqContext = searchReqContext_, searchReqMessage = searchReqMessage_}

buildSearchMessageV2 :: (Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m BecknV2.OnDemand.Types.SearchReqMessage
buildSearchMessageV2 origin destination distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber isReallocationEnabled = do
  searchReqMessageIntent_ <- tfIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled
  pure $ BecknV2.OnDemand.Types.SearchReqMessage {searchReqMessageIntent = searchReqMessageIntent_}

tfCustomer :: (Kernel.Types.App.MonadFlow m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m (Maybe BecknV2.OnDemand.Types.Customer)
tfCustomer customerLanguage disabilityTag mbPhoneNumber = do
  let customerContact_ = Nothing
  customerPerson_ <- tfPerson customerLanguage disabilityTag mbPhoneNumber
  let returnData = BecknV2.OnDemand.Types.Customer {customerContact = customerContact_, customerPerson = customerPerson_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfFulfillment :: (Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m (Maybe BecknV2.OnDemand.Types.Fulfillment)
tfFulfillment origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled = do
  let fulfillmentAgent_ = Nothing
  let fulfillmentId_ = Nothing
  let fulfillmentState_ = Nothing
  let fulfillmentStops_ = Beckn.OnDemand.Utils.Common.mkStops origin destination
  let fulfillmentTags_ = Beckn.OnDemand.Utils.Search.mkSearchFulFillmentTags distance duration mbPoints isReallocationEnabled
  let fulfillmentType_ = Nothing
  let fulfillmentVehicle_ = Nothing
  fulfillmentCustomer_ <- tfCustomer customerLanguage disabilityTag mbPhoneNumber
  let returnData = BecknV2.OnDemand.Types.Fulfillment {fulfillmentAgent = fulfillmentAgent_, fulfillmentCustomer = fulfillmentCustomer_, fulfillmentId = fulfillmentId_, fulfillmentState = fulfillmentState_, fulfillmentStops = fulfillmentStops_, fulfillmentTags = fulfillmentTags_, fulfillmentType = fulfillmentType_, fulfillmentVehicle = fulfillmentVehicle_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfIntent :: (Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m (Maybe BecknV2.OnDemand.Types.Intent)
tfIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled = do
  let intentTags_ = Nothing
  intentFulfillment_ <- tfFulfillment origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled
  intentPayment_ <- tfPayment "BPP"
  let returnData = BecknV2.OnDemand.Types.Intent {intentFulfillment = intentFulfillment_, intentPayment = intentPayment_, intentTags = intentTags_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfPayment :: (Kernel.Types.App.MonadFlow m) => Data.Text.Text -> m (Maybe BecknV2.OnDemand.Types.Payment)
tfPayment collectedBy = do
  let paymentCollectedBy_ = Just collectedBy
  let paymentId_ = Nothing
  let paymentParams_ = Nothing
  let paymentStatus_ = Nothing
  let paymentTags_ = Beckn.OnDemand.Utils.Common.mkPaymentTags
  let paymentType_ = Nothing
  let returnData = BecknV2.OnDemand.Types.Payment {paymentCollectedBy = paymentCollectedBy_, paymentId = paymentId_, paymentParams = paymentParams_, paymentStatus = paymentStatus_, paymentTags = paymentTags_, paymentType = paymentType_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData

tfPerson :: (Kernel.Types.App.MonadFlow m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m (Maybe BecknV2.OnDemand.Types.Person)
tfPerson customerLanguage disabilityTag mbPhoneNumber = do
  let personId_ = Nothing
  let personImage_ = Nothing
  let personName_ = Nothing
  let personTags_ = Beckn.OnDemand.Utils.Search.mkCustomerInfoTags customerLanguage disabilityTag mbPhoneNumber
  let returnData = BecknV2.OnDemand.Types.Person {personId = personId_, personImage = personImage_, personName = personName_, personTags = personTags_}
  let allNothing = BecknV2.OnDemand.Utils.Common.allNothing returnData
  if allNothing
    then pure Nothing
    else pure $ Just returnData
