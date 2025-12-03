module API.Internal.QuoteRespond
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.Driver as DDriver
import Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "driver"
      :> "quote"
      :> "respond"
      :> TokenAuth
      :> Header "x-package" Text
      :> Header "x-bundle-version" Version
      :> Header "x-client-version" Version
      :> Header "x-config-version" Version
      :> Header "x-react-bundle-version" Text
      :> Header "x-device" Text
      :> ReqBody '[JSON] DDriver.DriverRespondReq
      :> Post '[JSON] APISuccess
  )

handler :: FlowServer API
handler =
  respondQuote

respondQuote ::
  (Id SP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  DDriver.DriverRespondReq ->
  FlowHandler APISuccess
respondQuote (personId, driverId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbReactBundleVersion mbDevice = withFlowHandlerAPI . DDriver.respondQuote (personId, driverId, merchantOpCityId) clientId mbBundleVersion mbClientVersion mbConfigVersion mbReactBundleVersion mbDevice
