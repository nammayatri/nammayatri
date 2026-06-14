-- | Provider-agnostic dispatch for the partner embedded-auth integration.
-- Add a new provider by adding a constructor to PartnerAuthServiceConfig and a
-- case here. Mirrors ChatCompletion.Interface.
module PartnerAuth.Interface
  ( verifyToken,
    getUserDetails,
  )
where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Servant.Client (HasRequestId)
import qualified PartnerAuth.Interface.BHIM as BHIM
import PartnerAuth.Interface.Types
import qualified PartnerAuth.Types as PT

verifyToken :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => PartnerAuthServiceConfig -> Text -> m Bool
verifyToken serviceConfig token = case serviceConfig of
  BHIMConfig cfg -> BHIM.verifyToken cfg token

getUserDetails :: (EncFlow m r, CoreMetrics m, Log m, HasRequestId r, MonadReader r m) => PartnerAuthServiceConfig -> Text -> m PT.PartnerUserDetails
getUserDetails serviceConfig token = case serviceConfig of
  BHIMConfig cfg -> BHIM.getUserDetails cfg token
