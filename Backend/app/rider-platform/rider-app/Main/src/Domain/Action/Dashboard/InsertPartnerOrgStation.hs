{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.InsertPartnerOrgStation (postInsertPartnerOrgStationInsertPartnerOrgStation) where

import qualified API.Types.RiderPlatform.Management.InsertPartnerOrgStation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.PartnerOrgStation as TPOS
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Servant
import qualified Storage.Queries.PartnerOrgStation as QPOS
import Tools.Auth

postInsertPartnerOrgStationInsertPartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.InsertPartnerOrgStation.InsertData -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postInsertPartnerOrgStationInsertPartnerOrgStation _ _ req = do
  now <- getCurrentTime
  let newStation =
        TPOS.PartnerOrgStation
          { name = req.stationName,
            partnerOrgId = Kernel.Types.Id.Id req.partnerOrgID,
            partnerOrgStationId = Kernel.Types.Id.Id req.partnerOrgStationID,
            stationId = Kernel.Types.Id.Id req.stationID,
            createdAt = now,
            updatedAt = now
          }
  QPOS.create newStation
  pure Success
