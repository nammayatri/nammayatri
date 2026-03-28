module Domain.Action.Dashboard.Management.Toll
  ( getTollList,
    putTollUpdate,
  )
where

import qualified API.Types.ProviderPlatform.Management.Endpoints.Toll as Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.Toll as DT
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Toll as CQToll
import qualified Storage.Queries.Toll as QToll
import qualified Storage.Queries.TollExtra as QTollExtra
import Tools.Error

getTollList ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [Common.TollRes]
getTollList merchantShortId opCity = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  tolls <- CQToll.findAllTollsByMerchantOperatingCity merchantOpCity.id
  pure $ map mkTollRes tolls

putTollUpdate ::
  Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateTollReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putTollUpdate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  toll <- QToll.findByPrimaryKey (Id.Id req.tollId) >>= fromMaybeM (InvalidRequest $ "Toll not found: " <> req.tollId)
  when (toll.merchantOperatingCityId /= Just merchantOpCity.id) $
    throwError $ InvalidRequest "Toll does not belong to this merchant city"
  QTollExtra.updateEnabledById (Id.Id req.tollId) req.enabled
  CQToll.clearTollCache merchantOpCity.id
  pure Kernel.Types.APISuccess.Success

mkTollRes :: DT.Toll -> Common.TollRes
mkTollRes t =
  Common.TollRes
    { id = Id.getId t.id,
      name = t.name,
      enabled = t.enabled,
      isAutoRickshawAllowed = t.isAutoRickshawAllowed,
      isTwoWheelerAllowed = t.isTwoWheelerAllowed,
      createdAt = t.createdAt,
      updatedAt = t.updatedAt
    }
