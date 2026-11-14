module API.Internal.PaymentChargeDetails where

import BecknV2.OnDemand.Settlement (PaymentChargeDetails)
import Domain.Types.Extra.MerchantPaymentMethod (PaymentInstrument)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.InternalAuth as SIA
import qualified SharedLogic.Settlement as Settlement
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

type API =
  "paymentChargeDetails"
    :> Header "token" Text
    :> QueryParam' '[Required, Strict] "merchantId" Text
    :> QueryParam' '[Required, Strict] "city" Context.City
    :> QueryParam' '[Required, Strict] "paymentInstrument" PaymentInstrument
    :> Get '[JSON] (Maybe PaymentChargeDetails)

handler :: FlowServer API
handler = getPaymentChargeDetails

getPaymentChargeDetails :: Maybe Text -> Text -> Context.City -> PaymentInstrument -> FlowHandler (Maybe PaymentChargeDetails)
getPaymentChargeDetails token merchantShortId city paymentInstrument = withFlowHandlerAPI $ do
  internalAPIKey <- asks (.internalAPIKey)
  SIA.checkInternalKey "paymentChargeDetails" [internalAPIKey] token
  merchantOpCity <- CQMOC.findByMerchantShortIdAndCity (ShortId merchantShortId) city >>= fromMaybeM (MerchantOperatingCityNotFound $ merchantShortId <> "-" <> show city)
  Settlement.resolvePaymentChargeDetails merchantOpCity.id (Just paymentInstrument)
