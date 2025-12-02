module Lib.Dashcam.Domain.Interface where

import Data.Aeson
import Kernel.External.Encryption
import Kernel.Prelude hiding (error)
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Utils.Common (MonadFlow)
import Kernel.Utils.Servant.Client
import Lib.Dashcam.Domain.Cautio.Flow as Cautio
import Lib.Dashcam.Domain.Cautio.Types as Cautio

data DashCamServiceConfig = CautioConfig Cautio.CautioConfig
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

cautioInstallationStatus ::
  ( MonadFlow m,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Cautio.CautioConfig ->
  Cautio.InstallationStatusReq ->
  m [Cautio.InstallationRespEntity]
cautioInstallationStatus config req = do
  let baseUrl = config.url
      org_id = fromMaybe "" config.merchantId
  Cautio.cautioInstallationStatus baseUrl (req{params = mkCautioReqParams org_id})
  where
    mkCautioReqParams org_id = ParamsCautio (mkPathCautio org_id) req.params.querystring
    mkPathCautio org_id = PathCautio {organisation_id = org_id}
