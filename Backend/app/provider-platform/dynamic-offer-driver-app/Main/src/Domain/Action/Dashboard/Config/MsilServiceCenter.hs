module Domain.Action.Dashboard.Config.MsilServiceCenter
  ( listServiceCenters,
    createServiceCenter,
    updateServiceCenter,
    deleteServiceCenter,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MsilServiceCenter as DMSC
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.MsilServiceCenter as CQMsilServiceCenter
import qualified Storage.Queries.MsilServiceCenter as QMsilServiceCenter

-- | List all service centers for a merchant operating city (admin)
listServiceCenters ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m [DMSC.MsilServiceCenter]
listServiceCenters _merchantId merchantOpCityId = do
  QMsilServiceCenter.findAllByMerchantOperatingCityId merchantOpCityId

-- | Create a new service center (admin)
createServiceCenter ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.MsilServiceCenter ->
  m DMSC.MsilServiceCenter
createServiceCenter _merchantId merchantOpCityId center = do
  QMsilServiceCenter.create center
  CQMsilServiceCenter.clearCityCache merchantOpCityId
  return center

-- | Update an existing service center (admin)
updateServiceCenter ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DMSC.MsilServiceCenter ->
  DMSC.MsilServiceCenter ->
  m ()
updateServiceCenter _merchantId merchantOpCityId centerId updatedCenter = do
  now <- getCurrentTime
  QMsilServiceCenter.updateById
    updatedCenter.name
    updatedCenter.centerType
    updatedCenter.address
    updatedCenter.lat
    updatedCenter.lon
    updatedCenter.servicesOffered
    updatedCenter.phoneNumber
    updatedCenter.email
    updatedCenter.operatingHoursStart
    updatedCenter.operatingHoursEnd
    updatedCenter.isOpenSunday
    updatedCenter.isActive
    updatedCenter.city
    updatedCenter.state
    updatedCenter.pincode
    now
    centerId
  CQMsilServiceCenter.clearCityCache merchantOpCityId

-- | Delete a service center (admin) - soft delete by setting isActive = False
deleteServiceCenter ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id DMSC.MsilServiceCenter ->
  m ()
deleteServiceCenter _merchantId merchantOpCityId centerId = do
  QMsilServiceCenter.deleteById centerId
  CQMsilServiceCenter.clearCityCache merchantOpCityId
