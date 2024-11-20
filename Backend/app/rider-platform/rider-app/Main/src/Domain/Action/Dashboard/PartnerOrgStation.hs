{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.PartnerOrgStation
  ( postPartnerOrgStationCreatePartnerOrgStation,
    postPartnerOrgStationUpdatePartnerOrgStation,
  )
where

import qualified API.Types.RiderPlatform.Management.PartnerOrgStation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.PartnerOrgStation as TPOS
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CST
import qualified Storage.Queries.PartnerOrgStation as QPOS
import Tools.Auth

postPartnerOrgStationCreatePartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.PartnerOrgStation.CreatePartnerOrgStationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPartnerOrgStationCreatePartnerOrgStation merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  CST.findByStationIdAndMerchantOperatingCityId merchantOpCity.id (Kernel.Types.Id.Id req.stationID) >>= \case
    Just _ -> do
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
    Nothing -> throwError AccessDenied

postPartnerOrgStationUpdatePartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.PartnerOrgStation.UpdatePartnerOrgStationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postPartnerOrgStationUpdatePartnerOrgStation merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  getStation <- CQPOS.findByPOrgIdAndPOrgStationId (Kernel.Types.Id.Id req.partnerOrgID) (Kernel.Types.Id.Id req.oldPartnerOrgStationID)
  case getStation of
    Just station -> do
      CST.findByStationIdAndMerchantOperatingCityId merchantOpCity.id station.stationId >>= \case
        Just _ -> do
          QPOS.updatePartnerOrgStationId req.oldPartnerOrgStationID req.newPartnerOrgStationID req.partnerOrgID
          CQPOS.clearCache (Kernel.Types.Id.Id req.partnerOrgID) (Kernel.Types.Id.Id req.oldPartnerOrgStationID)
          pure Success
        Nothing -> throwError AccessDenied
    Nothing -> throwError AccessDenied
