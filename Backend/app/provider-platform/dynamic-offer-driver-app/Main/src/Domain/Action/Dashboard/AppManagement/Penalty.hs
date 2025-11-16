{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.Penalty (postPenaltyTriggerJobCancellationPenaltyServiceName) where

import qualified Dashboard.Common as Common
import Data.OpenApi (ToSchema)
import Domain.Action.Dashboard.Common (mapServiceName)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Plan as DTP
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess as API
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import SharedLogic.Allocator.Jobs.DriverFeeUpdates.DriverFee (updateCancellationPenaltyAccumulationFees)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Auth
import Tools.Error

postPenaltyTriggerJobCancellationPenaltyServiceName :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Common.ServiceNames -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPenaltyTriggerJobCancellationPenaltyServiceName merchantShortId opCity serviceName' = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let serviceName = mapServiceName serviceName'
  updateCancellationPenaltyAccumulationFees serviceName transporterConfig merchant.id merchantOpCityId
  return $ API.Success
