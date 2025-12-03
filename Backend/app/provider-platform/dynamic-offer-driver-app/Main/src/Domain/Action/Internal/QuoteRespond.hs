module Domain.Action.Internal.QuoteRespond where

import qualified Domain.Action.UI.Driver as DDriver
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Version (Version)
import Kernel.Utils.Common
import Storage.Beam.SystemConfigs ()
import Tools.Auth

respondQuote ::
  Maybe RegToken ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  DDriver.DriverRespondReq ->
  Flow APISuccess
respondQuote token clientId mbBundleVersion mbClientVersion mbConfigVersion mbReactBundleVersion mbDevice request = do
  regToken <- fromMaybeM AccessDenied token
  (personId, driverId, merchantOpCityId) <- verifyPerson regToken
  DDriver.respondQuote
    (personId, driverId, merchantOpCityId)
    clientId
    mbBundleVersion
    mbClientVersion
    mbConfigVersion
    mbReactBundleVersion
    mbDevice
    request
