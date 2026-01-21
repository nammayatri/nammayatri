module Domain.Action.RiderPlatform.RideBooking.Confirm (postConfirmRideSearchQuotes) where

import qualified API.Client.RiderPlatform.RideBooking
import qualified "rider-app" API.UI.Confirm
import qualified "rider-app" Domain.Types.Extra.MerchantPaymentMethod
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.Quote
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

postConfirmRideSearchQuotes :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Quote.Quote -> Kernel.Prelude.Maybe Kernel.External.Payment.Interface.PaymentMethodId -> Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument -> Maybe Bool -> Environment.Flow API.UI.Confirm.ConfirmRes
postConfirmRideSearchQuotes merchantShortId opCity apiTokenInfo customerId quoteId paymentMethodId paymentInstrument isAdvanceBookingEnabled = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.confirmDSL.postConfirmRideSearchQuotes) customerId quoteId (Just apiTokenInfo.personId.getId) paymentMethodId paymentInstrument isAdvanceBookingEnabled
