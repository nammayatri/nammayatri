{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ApprovalRequestExtra where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Driver as Common
import qualified Domain.Types.ApprovalRequest as DTR
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ApprovalRequest as BeamDR
import Storage.Queries.OrphanInstances.ApprovalRequest

-- Extra code goes here --
findDriverRequestsByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe DTR.RequestStatus -> Maybe Int -> Maybe Int -> m [DTR.ApprovalRequest]
findDriverRequestsByFleetOwnerId merchantOpCityId fleetOwnerId mbFrom mbTo mbStatus mbLimit mbOffset = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamDR.merchantOperatingCityId $ Se.Eq merchantOpCityId.getId]
            <> [Se.Is BeamDR.requesteeId $ Se.Eq fleetOwnerId]
            <> [Se.Is BeamDR.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamDR.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is BeamDR.status $ Se.Eq (fromJust mbStatus) | isJust mbStatus]
        )
    ]
    (Se.Desc BeamDR.createdAt)
    (Just limitVal)
    (Just offsetVal)
  where
    limitVal = min (fromMaybe 10 mbLimit) 10
    offsetVal = fromMaybe 0 mbOffset
