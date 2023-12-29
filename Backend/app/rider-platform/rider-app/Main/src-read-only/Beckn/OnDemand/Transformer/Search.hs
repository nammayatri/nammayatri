{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.Search where

import qualified Beckn.OnDemand.Utils.Search
import qualified BecknV2.OnDemand.Types
import qualified Data.Text
import qualified Domain.Action.UI.Search.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.SearchRequest
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Logging
import qualified Kernel.Types.Time
import qualified Tools.Maps

buildSearchReqV2 :: (Monad m, EulerHS.Prelude.MonadThrow m, Kernel.Types.Time.MonadTime m, Kernel.Types.Logging.Log m) => Maybe Tools.Maps.Language -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Data.Text.Text -> Maybe [Tools.Maps.LatLong] -> Domain.Action.UI.Search.Common.SearchReqLocation -> m BecknV2.OnDemand.Types.SearchReqMessage
buildSearchReqV2 customerLanguage destination disabilityTag distance duration mbPhoneNumber mbPoints origin = do
  searchReqMessageSearchReqMessageIntent <- Just <$> tfIntent customerLanguage destination disabilityTag distance duration mbPhoneNumber mbPoints origin
  pure $
    BecknV2.OnDemand.Types.SearchReqMessage
      { searchReqMessageIntent = searchReqMessageSearchReqMessageIntent
      }

tfCustomer :: (Monad m, EulerHS.Prelude.MonadThrow m, Kernel.Types.Time.MonadTime m, Kernel.Types.Logging.Log m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Customer
tfCustomer customerLanguage disabilityTag mbPhoneNumber = do
  let customerCustomerContact = Nothing
  customerCustomerPerson <- Just <$> tfPerson customerLanguage disabilityTag mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.Customer
      { customerContact = customerCustomerContact,
        customerPerson = customerCustomerPerson
      }

tfFulfillment :: (Monad m, EulerHS.Prelude.MonadThrow m, Kernel.Types.Time.MonadTime m, Kernel.Types.Logging.Log m) => Maybe Tools.Maps.Language -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Data.Text.Text -> Maybe [Tools.Maps.LatLong] -> Domain.Action.UI.Search.Common.SearchReqLocation -> m BecknV2.OnDemand.Types.Fulfillment
tfFulfillment customerLanguage destination disabilityTag distance duration mbPhoneNumber mbPoints origin = do
  let fulfillmentFulfillmentAgent = Nothing
  let fulfillmentFulfillmentState = Nothing
  let fulfillmentFulfillmentStops = Beckn.OnDemand.Utils.Search.mkStops origin destination
  let fulfillmentFulfillmentTags = Beckn.OnDemand.Utils.Search.mkRouteInfoTags distance duration mbPoints
  let fulfillmentFulfillmentText = Nothing
  let fulfillmentFulfillmentType = Nothing
  let fulfillmentFulfillmentVehicle = Nothing
  fulfillmentFulfillmentCustomer <- Just <$> tfCustomer customerLanguage disabilityTag mbPhoneNumber
  pure $
    BecknV2.OnDemand.Types.Fulfillment
      { fulfillmentAgent = fulfillmentFulfillmentAgent,
        fulfillmentCustomer = fulfillmentFulfillmentCustomer,
        fulfillmentState = fulfillmentFulfillmentState,
        fulfillmentStops = fulfillmentFulfillmentStops,
        fulfillmentTags = fulfillmentFulfillmentTags,
        fulfillmentText = fulfillmentFulfillmentText,
        fulfillmentType = fulfillmentFulfillmentType,
        fulfillmentVehicle = fulfillmentFulfillmentVehicle
      }

tfIntent :: (Monad m, EulerHS.Prelude.MonadThrow m, Kernel.Types.Time.MonadTime m, Kernel.Types.Logging.Log m) => Maybe Tools.Maps.Language -> Domain.Action.UI.Search.Common.SearchReqLocation -> Maybe Data.Text.Text -> Maybe Kernel.Types.Common.Meters -> Maybe Kernel.Types.Common.Seconds -> Maybe Data.Text.Text -> Maybe [Tools.Maps.LatLong] -> Domain.Action.UI.Search.Common.SearchReqLocation -> m BecknV2.OnDemand.Types.Intent
tfIntent customerLanguage destination disabilityTag distance duration mbPhoneNumber mbPoints origin = do
  let intentIntentPayment = Nothing
  let intentIntentTags = Nothing
  intentIntentFulfillment <- Just <$> tfFulfillment customerLanguage destination disabilityTag distance duration mbPhoneNumber mbPoints origin
  pure $
    BecknV2.OnDemand.Types.Intent
      { intentFulfillment = intentIntentFulfillment,
        intentPayment = intentIntentPayment,
        intentTags = intentIntentTags
      }

tfPerson :: (Monad m, EulerHS.Prelude.MonadThrow m, Kernel.Types.Time.MonadTime m, Kernel.Types.Logging.Log m) => Maybe Tools.Maps.Language -> Maybe Data.Text.Text -> Maybe Data.Text.Text -> m BecknV2.OnDemand.Types.Person
tfPerson customerLanguage disabilityTag mbPhoneNumber = do
  let personPersonName = Nothing
  let personPersonTags = Beckn.OnDemand.Utils.Search.mkCustomerInfoTags customerLanguage disabilityTag mbPhoneNumber
  let personPersonText = Nothing
  pure $
    BecknV2.OnDemand.Types.Person
      { personName = personPersonName,
        personTags = personPersonTags,
        personText = personPersonText
      }
