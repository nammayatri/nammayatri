module Lib.Dashcam.Domain.Cautio.Flow where

import EulerHS.Types as Euler
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (fromEitherM)
import Kernel.Utils.Servant.Client
import qualified Lib.Dashcam.Domain.Cautio.Types as Cautio
import Servant hiding (throwError)

type CautioInstallationStatus =
  "api"
    :> "namma-yatri"
    :> "installation-status"
    :> ReqBody '[JSON] Cautio.InstallationStatusReq
    :> Post '[JSON] [Cautio.InstallationRespEntity]

cautioInstallationStatus ::
  ( Metrics.CoreMetrics m,
    MonadFlow m,
    EncFlow m r,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Cautio.InstallationStatusReq ->
  m [Cautio.InstallationRespEntity]
cautioInstallationStatus url req = do
  let proxy = Proxy @CautioInstallationStatus
      eulerClient = Euler.client proxy req
  callAPI url eulerClient "installation-status" proxy
    >>= fromEitherM (\err -> InternalError $ "Failed to call " <> "cautio installation" <> " API: " <> show err)
