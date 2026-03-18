module Domain.Action.UI.MsilServiceCenter
  ( getDriverMsilServiceCenters,
    getDriverMsilServiceCenterDetail,
    postDriverMsilServiceCenterCallback,
  )
where

import qualified API.Types.UI.MsilServiceCenter as API
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MsilServiceCenter as DMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ServiceCenterCallbackRequest as DSCR
import Environment
import Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MsilServiceCenter as QMsilServiceCenter
import qualified Storage.Queries.MsilServiceCenterExtra as QMsilServiceCenterExtra
import qualified Storage.Queries.ServiceCenterCallbackRequest as QServiceCenterCallback

-- | Get nearby MSIL service centers based on driver location, with optional filters
getDriverMsilServiceCenters ::
  ( ( Maybe (Id DP.Person),
      Id Domain.Types.Merchant.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Double ->
    Double ->
    Maybe Int ->
    Maybe API.CenterType ->
    Maybe API.ServiceType ->
    Maybe Int ->
    Flow API.GetServiceCentersResp
  )
getDriverMsilServiceCenters (_, _, merchantOpCityId) lat lon mbRadius mbCenterType _mbServiceType mbLimit = do
  let radius = fromMaybe 10000 mbRadius
      limit = fromMaybe 20 mbLimit
      centerTypeFilter = fmap show mbCenterType
  centers <- QMsilServiceCenterExtra.findNearbyActiveCenters merchantOpCityId lat lon radius centerTypeFilter limit
  now <- getCurrentTime
  let items = map (mkServiceCenterItem now) centers
  return
    API.GetServiceCentersResp
      { serviceCenters = items,
        totalCount = length items
      }

mkServiceCenterItem :: UTCTime -> (DMSC.MsilServiceCenter, Maybe Int) -> API.ServiceCenterItem
mkServiceCenterItem now (center, mbDist) =
  API.ServiceCenterItem
    { id = center.id,
      name = center.name,
      centerType = parseCenterType center.centerType,
      address = center.address,
      lat = center.lat,
      lon = center.lon,
      distance = mbDist,
      servicesOffered = map parseServiceType center.servicesOffered,
      phoneNumber = center.phoneNumber,
      operatingHoursStart = center.operatingHoursStart,
      operatingHoursEnd = center.operatingHoursEnd,
      isOpenSunday = center.isOpenSunday,
      isCurrentlyOpen = checkIfOpen now center
    }

parseCenterType :: Text -> API.CenterType
parseCenterType "WORKSHOP" = API.WORKSHOP
parseCenterType "BODYSHOP" = API.BODYSHOP
parseCenterType _ = API.BOTH

parseServiceType :: Text -> API.ServiceType
parseServiceType "GENERAL_SERVICE" = API.GENERAL_SERVICE
parseServiceType "AC_REPAIR" = API.AC_REPAIR
parseServiceType "BODY_WORK" = API.BODY_WORK
parseServiceType "PAINTING" = API.PAINTING
parseServiceType "DENTING" = API.DENTING
parseServiceType "WHEEL_ALIGNMENT" = API.WHEEL_ALIGNMENT
parseServiceType "ELECTRICAL" = API.ELECTRICAL
parseServiceType "TIRE_SERVICE" = API.TIRE_SERVICE
parseServiceType _ = API.GENERAL_SERVICE

checkIfOpen :: UTCTime -> DMSC.MsilServiceCenter -> Bool
checkIfOpen _now _center = True -- Simplified: production implementation would check operating hours against current time

-- | Get details for a specific service center
getDriverMsilServiceCenterDetail ::
  ( ( Maybe (Id DP.Person),
      Id Domain.Types.Merchant.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Id DMSC.MsilServiceCenter ->
    Flow API.ServiceCenterDetailResp
  )
getDriverMsilServiceCenterDetail (_, _, _) centerId = do
  center <- QMsilServiceCenter.findById centerId >>= fromMaybeM (InvalidRequest "Service center not found")
  now <- getCurrentTime
  return
    API.ServiceCenterDetailResp
      { id = center.id,
        name = center.name,
        centerType = parseCenterType center.centerType,
        address = center.address,
        lat = center.lat,
        lon = center.lon,
        servicesOffered = map parseServiceType center.servicesOffered,
        brand = center.brand,
        dealerCode = center.dealerCode,
        phoneNumber = center.phoneNumber,
        email = center.email,
        operatingHoursStart = center.operatingHoursStart,
        operatingHoursEnd = center.operatingHoursEnd,
        isOpenSunday = center.isOpenSunday,
        isCurrentlyOpen = checkIfOpen now center,
        city = center.city,
        state = center.state,
        pincode = center.pincode
      }

-- | Submit a callback request for a service center
postDriverMsilServiceCenterCallback ::
  ( ( Maybe (Id DP.Person),
      Id Domain.Types.Merchant.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Id DMSC.MsilServiceCenter ->
    API.CallbackReq ->
    Flow API.CallbackResp
  )
postDriverMsilServiceCenterCallback (mbDriverId, _, merchantOpCityId) centerId req = do
  driverId <- mbDriverId & fromMaybeM (InvalidRequest "Driver not authenticated")
  _center <- QMsilServiceCenter.findById centerId >>= fromMaybeM (InvalidRequest "Service center not found")
  now <- getCurrentTime
  callbackId <- generateGUID
  let callbackRequest =
        DSCR.ServiceCenterCallbackRequest
          { id = callbackId,
            driverId = driverId,
            serviceCenterId = centerId,
            vehicleNumber = req.vehicleNumber,
            issueDescription = req.issueDescription,
            preferredDate = req.preferredDate,
            preferredTimeSlot = req.preferredTimeSlot >>= Just . show,
            status = "PENDING",
            adminNotes = Nothing,
            merchantOperatingCityId = merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }
  QServiceCenterCallback.create callbackRequest
  return
    API.CallbackResp
      { callbackRequestId = callbackId,
        result = "Success",
        message = "Callback request submitted. The service center will contact you within 24 hours."
      }
