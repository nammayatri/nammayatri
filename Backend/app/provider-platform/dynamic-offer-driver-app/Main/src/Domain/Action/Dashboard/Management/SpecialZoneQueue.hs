{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Management.SpecialZoneQueue (postSpecialZoneQueueTriggerNotify) where

import qualified API.Types.ProviderPlatform.Management.SpecialZoneQueue
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Queries.GateInfo as QGI
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

postSpecialZoneQueueTriggerNotify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.SpecialZoneQueue.TriggerSpecialZoneQueueNotifyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSpecialZoneQueueTriggerNotify merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  gate <- Esq.runInReplica (QGI.findById (Kernel.Types.Id.Id req.gateId)) >>= fromMaybeM (InvalidRequest $ "Gate not found: " <> req.gateId)
  notifiedCount <- SpecialZoneDriverDemand.forceNotifyDriverDemand merchantOpCity.id merchant.id gate req.vehicleType req.driversToNotify
  logInfo $ "Dashboard trigger: notified " <> show notifiedCount <> " drivers for gate " <> req.gateId
  pure Kernel.Types.APISuccess.Success
