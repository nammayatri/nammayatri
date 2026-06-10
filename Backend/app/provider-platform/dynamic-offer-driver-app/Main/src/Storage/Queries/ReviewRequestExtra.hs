{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ReviewRequestExtra where

import Control.Applicative ((<|>))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.ReviewRequest as DRR
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id (Id, getId)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReviewRequest as Beam
import Storage.Queries.OrphanInstances.ReviewRequest

findAllByFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Int ->
  Int ->
  DRR.EntityType ->
  Maybe [Text] ->
  Maybe Text ->
  Maybe DRR.RequestStatus ->
  Maybe DRR.RequestType ->
  m [DRR.ReviewRequest]
findAllByFilters merchantId merchantOpCityId mbFrom mbTo limitAmount offsetAmount entityType mbEntityIds mbRcNo mbStatusDomain mbReqType = do
  case mbEntityIds of
    Just [] -> pure []
    _ -> do
      let conditions =
            catMaybes
              [ Just $ Se.Is Beam.merchantId (Se.Eq (getId merchantId)),
                Just $ Se.Is Beam.merchantOperatingCityId (Se.Eq (getId merchantOpCityId)),
                Just $ Se.Is Beam.entityType (Se.Eq entityType),
                (Se.Is Beam.createdAt . Se.GreaterThanOrEq) <$> mbFrom,
                (Se.Is Beam.createdAt . Se.LessThanOrEq) <$> mbTo,
                (Se.Is Beam.entityId . Se.In) <$> mbEntityIds,
                (Se.Is Beam.rcNo . Se.Eq . Just) <$> mbRcNo,
                (Se.Is Beam.requestStatus . Se.Eq) <$> mbStatusDomain,
                (Se.Is Beam.requestType . Se.Eq) <$> mbReqType
              ]
      findAllWithOptionsKV conditions (Se.Desc Beam.createdAt) (Just limitAmount) (Just offsetAmount)

-- | Latest review request for an entity (any status), used to derive the BotApproval doc status.
findLatestByEntityAndType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  DRR.EntityType ->
  DRR.RequestType ->
  Maybe Text ->
  m (Maybe DRR.ReviewRequest)
findLatestByEntityAndType entityId entityType requestType mbRcNo =
  listToMaybe
    <$> findAllWithOptionsKV
      [ Se.And $
          [ Se.Is Beam.entityId $ Se.Eq entityId,
            Se.Is Beam.entityType $ Se.Eq entityType,
            Se.Is Beam.requestType $ Se.Eq requestType
          ]
            <> [Se.Is Beam.rcNo $ Se.Eq (Just rcNo) | Just rcNo <- [mbRcNo]]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      Nothing
