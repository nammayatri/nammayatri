module ConfigPilotFrontend.Flow where

import qualified ConfigPilotFrontend.API as CPI
import ConfigPilotFrontend.Types
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common

configValidate :: (CoreMetrics m, MonadFlow m, HasRequestId r, MonadReader r m) => BaseUrl -> Value -> m TSServiceValidateResp
configValidate url req = do
  callAPI url (CPI.tsServiceValidate req) "tsServiceValidate" CPI.tsServiceValidateAPI
    >>= fromEitherM (ExternalAPICallError (Just "UNABLE_TO_CALL_TS_SERVICE_VALIDATE_API") url)
