{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Search
import qualified BecknV2.OnDemand.Types
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

buildBecknSearchReqV2 :: (Monad m, Kernel.Types.App.MonadFlow m) => Kernel.Types.Beckn.Context.Action -> Kernel.Types.Beckn.Context.Domain -> Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Domain.Types.Merchant.Merchant -> Kernel.Prelude.BaseUrl -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.SearchReq
buildBecknSearchReqV2 action domain origin destination searchId distance duration customerLanguage disabilityTag merchant bapUri mbPoints mbPhoneNumber = do
  searchReqSearchReqContext <- Beckn.OnDemand.Utils.Common.buildContextV2 action domain searchId.getId (Just searchId.getId) merchant.bapId bapUri Nothing Nothing merchant.defaultCity merchant.country
  searchReqSearchReqMessage <- buildSearchMessageV2 origin destination distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.SearchReq
      { searchReqContext = searchReqSearchReqContext,
        searchReqMessage = searchReqSearchReqMessage
      }

buildSearchMessageV2 :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.SearchReqMessage
buildSearchMessageV2 origin destination distance duration customerLanguage disabilityTag mbPoints mbPhoneNumber = do
  searchReqMessageSearchReqMessageIntent <- Just <$> tfIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.SearchReqMessage
      { searchReqMessageIntent = searchReqMessageSearchReqMessageIntent
      }

tfCustomer :: (Monad m, Kernel.Types.App.MonadFlow m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Customer
tfCustomer customerLanguage disabilityTag mbPhoneNumber = do
  let customerCustomerContact = Nothing
  customerCustomerPerson <- Just <$> tfPerson customerLanguage disabilityTag mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.Customer
      { customerContact = customerCustomerContact,
        customerPerson = customerCustomerPerson
      }

tfFulfillment :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Fulfillment
tfFulfillment origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber = do
  let fulfillmentFulfillmentAgent = Nothing
  let fulfillmentFulfillmentId = Nothing
  let fulfillmentFulfillmentState = Nothing
  let fulfillmentFulfillmentStops = Beckn.OnDemand.Utils.Common.mkStops origin destination
  let fulfillmentFulfillmentTags = Beckn.OnDemand.Utils.Search.mkRouteInfoTags distance duration mbPoints
  let fulfillmentFulfillmentType = Nothing
  let fulfillmentFulfillmentVehicle = Nothing
  fulfillmentFulfillmentCustomer <- Just <$> tfCustomer customerLanguage disabilityTag mbPhoneNumber
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

tfIntent :: (Monad m, Kernel.Types.App.MonadFlow m) => Domain.Action.UI.Search.Common.SearchReqLocation -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe [Tools.Maps.LatLong] -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Intent
tfIntent origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber = do
  let intentIntentPayment = Nothing
  let intentIntentTags = Nothing
  intentIntentFulfillment <- Just <$> tfFulfillment origin destination customerLanguage disabilityTag distance duration mbPoints mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.Intent
      { intentFulfillment = intentIntentFulfillment,
        intentPayment = intentIntentPayment,
        intentTags = intentIntentTags
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
