{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.UpdatePartnerOrgStationID (postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID) where

import qualified API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, withFlowHandlerAPI')
import Servant
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.Queries.PartnerOrgStation as QPOS
import Tools.Auth

postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.ReqData -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID _ _ req = do
  QPOS.updatePartnerOrgStationId req.oldPartnerOrgStationID req.newPartnerOrgStationID req.partnerOrgID
  CQPOS.deletePartnerOrgStation (Kernel.Types.Id.Id req.partnerOrgID) (Kernel.Types.Id.Id req.oldPartnerOrgStationID)
  pure Success
