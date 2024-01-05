{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified BecknV2.OnDemand.Types
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

buildBecknSearchReqV2 :: (Monad m, Kernel.Types.App.MonadFlow m) => Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Domain.Types.Merchant.Merchant -> Kernel.Prelude.BaseUrl -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m BecknV2.OnDemand.Types.SearchReq
buildBecknSearchReqV2 action domain origin destination searchId distance duration customerLanguage disabilityTag merchant bapUri mbPoints mbPhoneNumber isReallocationEnabled = do
  searchReqSearchReqContext <- BecknV2.OnDemand.Utils.Context.buildContextV2 action domain searchId.getId (Just searchId.getId) merchant.bapId bapUri Nothing Nothing merchant.defaultCity merchant.country
  searchReqSearchReqMessage <- buildSearchMessageV2 origin destination distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber isReallocationEnabled
  pure $
    BecknV2.OnDemand.Types.SearchReq
      { searchReqContext = searchReqSearchReqContext,
        searchReqMessage = searchReqSearchReqMessage
      }

buildSearchMessageV2 :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m BecknV2.OnDemand.Types.SearchReqMessage
buildSearchMessageV2 origin destination distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber isReallocationEnabled = do
  searchReqMessageSearchReqMessageIntent <- tfIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled <&> Just
  pure $
    BecknV2.OnDemand.Types.SearchReqMessage
      { searchReqMessageIntent = searchReqMessageSearchReqMessageIntent
      }

tfCustomer :: (Monad m, Kernel.Types.App.MonadFlow m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Customer
tfCustomer customerLanguage disabilityTag mbPhoneNumber = do
  let customerCustomerContact = Nothing
  customerCustomerPerson <- tfPerson customerLanguage disabilityTag mbPhoneNumber <&> Just
  pure $
    BecknV2.OnDemand.Types.Customer
      { customerContact = customerCustomerContact,
        customerPerson = customerCustomerPerson
      }

tfFulfillment :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m BecknV2.OnDemand.Types.Fulfillment
tfFulfillment origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled = do
  let fulfillmentFulfillmentAgent = Nothing
  let fulfillmentFulfillmentId = Nothing
  let fulfillmentFulfillmentState = Nothing
  let fulfillmentFulfillmentStops = Beckn.OnDemand.Utils.Common.mkStops origin destination
  let fulfillmentFulfillmentTags = Beckn.OnDemand.Utils.Search.mkSearchFulFillmentTags distance duration mbPoints isReallocationEnabled
  let fulfillmentFulfillmentType = Nothing
  let fulfillmentFulfillmentVehicle = Nothing
  fulfillmentFulfillmentCustomer <- tfCustomer customerLanguage disabilityTag mbPhoneNumber <&> Just
  pure $
    BecknV2.OnDemand.Types.Fulfillment
      { fulfillmentAgent = fulfillmentFulfillmentAgent,
        fulfillmentCustomer = fulfillmentFulfillmentCustomer,
        fulfillmentId = fulfillmentFulfillmentId,
        fulfillmentState = fulfillmentFulfillmentState,
        fulfillmentStops = fulfillmentFulfillmentStops,
        fulfillmentTags = fulfillmentFulfillmentTags,
        fulfillmentType = fulfillmentFulfillmentType,
        fulfillmentVehicle = fulfillmentFulfillmentVehicle
      }

tfIntent :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> Maybe Kernel.Prelude.Bool -> m BecknV2.OnDemand.Types.Intent
tfIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled = do
  let intentIntentTags = Nothing
  intentIntentFulfillment <- tfFulfillment origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber isReallocationEnabled <&> Just
  intentIntentPayment <- tfPayment "BPP" <&> Just
  pure $
    BecknV2.OnDemand.Types.Intent
      { intentFulfillment = intentIntentFulfillment,
        intentPayment = intentIntentPayment,
        intentTags = intentIntentTags
      }

tfPayment :: (Monad m, Kernel.Types.App.MonadFlow m) => Data.Text.Text -> m BecknV2.OnDemand.Types.Payment
tfPayment collectedBy = do
  let paymentPaymentCollectedBy = Just collectedBy
  let paymentPaymentId = Nothing
  let paymentPaymentParams = Nothing
  let paymentPaymentStatus = Nothing
  let paymentPaymentTags = Beckn.OnDemand.Utils.Common.mkPaymentTags
  let paymentPaymentType = Nothing
  pure $
    BecknV2.OnDemand.Types.Payment
      { paymentCollectedBy = paymentPaymentCollectedBy,
        paymentId = paymentPaymentId,
        paymentParams = paymentPaymentParams,
        paymentStatus = paymentPaymentStatus,
        paymentTags = paymentPaymentTags,
        paymentType = paymentPaymentType
      }

tfPerson :: (Monad m, Kernel.Types.App.MonadFlow m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Person
tfPerson customerLanguage disabilityTag mbPhoneNumber = do
  let personPersonId = Nothing
  let personPersonName = Nothing
  let personPersonTags = Beckn.OnDemand.Utils.Search.mkCustomerInfoTags customerLanguage disabilityTag mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.Person
      { personId = personPersonId,
        personName = personPersonName,
        personTags = personPersonTags
      }
