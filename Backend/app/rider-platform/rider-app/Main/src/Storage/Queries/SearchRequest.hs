{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SearchRequest where

import Data.Ord
import Data.Text (strip)
import qualified Domain.Types.Location as DL
import Domain.Types.Merchant.MerchantPaymentMethod (MerchantPaymentMethod)
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSSL
import qualified Domain.Types.SearchRequestMapping as DSRM
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import qualified SharedLogic.SearchRequestMapping as SSRM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.SearchRequestMapping as QSRM
import qualified Storage.Queries.SearchRequestMapping as SRM

createDSReq' :: MonadFlow m => SearchRequest -> m ()
createDSReq' = createWithKV

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SearchRequest -> m ()
create dsReq = do
  _ <- whenNothingM_ (QL.findById dsReq.fromLocation.id) $ do QL.create dsReq.fromLocation
  _ <- whenJust dsReq.toLocation $ \location -> processLocation location
  createDSReq' dsReq
  where
    processLocation location = whenNothingM_ (QL.findById location.id) $ do QL.create location

createDSReq :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SearchRequest -> m ()
createDSReq searchRequest = do
  fromLocationMap <- SSRM.buildPickUpLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DSRM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId)
  mbToLocationMap <- maybe (pure Nothing) (\detail -> Just <$> SSRM.buildDropLocationMapping detail.id searchRequest.id.getId DSRM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId)) searchRequest.toLocation
  void $ SRM.create fromLocationMap
  void $ whenJust mbToLocationMap $ \toLocMap -> SRM.create toLocMap
  create searchRequest

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

findByPersonId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId (Id personId) (Id searchRequestId) = findOneWithKV [Se.And [Se.Is BeamSR.id $ Se.Eq searchRequestId, Se.Is BeamSR.riderId $ Se.Eq personId]]

findAllByPerson :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [SearchRequest]
findAllByPerson (Id personId) = findAllWithKV [Se.Is BeamSR.riderId $ Se.Eq personId]

findLatestSearchRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe SearchRequest)
findLatestSearchRequest (Id riderId) = findAllWithOptionsKV [Se.Is BeamSR.riderId $ Se.Eq riderId] (Se.Desc BeamSR.createdAt) (Just 1) Nothing <&> listToMaybe

updateCustomerExtraFeeAndPaymentMethod :: MonadFlow m => Id SearchRequest -> Maybe Money -> Maybe (Id DMPM.MerchantPaymentMethod) -> m ()
updateCustomerExtraFeeAndPaymentMethod (Id searchReqId) customerExtraFee paymentMethodId =
  updateOneWithKV
    [ Se.Set BeamSR.customerExtraFee customerExtraFee,
      Se.Set BeamSR.selectedPaymentMethodId (getId <$> paymentMethodId)
    ]
    [Se.Is BeamSR.id (Se.Eq searchReqId)]

updateAutoAssign :: MonadFlow m => Id SearchRequest -> Bool -> Bool -> m ()
updateAutoAssign (Id searchRequestId) autoAssignedEnabled autoAssignedEnabledV2 = do
  updateOneWithKV
    [ Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled,
      Se.Set BeamSR.autoAssignEnabledV2 $ Just autoAssignedEnabledV2
    ]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

updatePaymentMethods :: MonadFlow m => Id SearchRequest -> [Id MerchantPaymentMethod] -> m ()
updatePaymentMethods (Id searchReqId) availablePaymentMethods =
  updateOneWithKV
    [ Se.Set BeamSR.availablePaymentMethods (getId <$> availablePaymentMethods)
    ]
    [Se.Is BeamSR.id (Se.Eq searchReqId)]

instance FromTType' BeamSR.SearchRequest SearchRequest where
  fromTType' BeamSR.SearchRequestT {..} = do
    bundleVersion' <- mapM readVersion (strip <$> bundleVersion)
    clientVersion' <- mapM readVersion (strip <$> clientVersion)
    mappings <- QSRM.findByEntityId id
    logDebug $ "sb-mappings-rider" <> show mappings
    (fl, tl) <-
      if null mappings -- HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME
        then do
          logInfo "Accessing Search Request Mapping Table"
          pickupLoc <- upsertFromLocationAndMappingForOldData id merchantId merchantOperatingCityId
          dropLoc <- upsertToLocationAndMappingForOldData id merchantId merchantOperatingCityId
          return (pickupLoc, Just dropLoc)
        else do
          let fromLocationMapping = filter (\loc -> loc.order == 0) mappings
              toLocationMappings = filter (\loc -> loc.order /= 0) mappings
          fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
          fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in search request for fromLocationId: " <> fromLocMap.locationId.getId)
          when (null toLocationMappings) $ throwError (InternalError "Entity Mappings For ToLocation Not Found")
          tl <-
            if null toLocationMappings
              then return Nothing
              else do
                let toLocMap = maximumBy (comparing (.order)) toLocationMappings
                QL.findById toLocMap.locationId
          return (fl, tl)
    merchantOperatingCityId' <- backfillMOCId merchantOperatingCityId
    pure $
      Just
        SearchRequest
          { id = Id id,
            startTime = startTime,
            validTill = validTill,
            riderId = Id riderId,
            fromLocation = fl,
            toLocation = tl,
            distance = HighPrecMeters <$> distance,
            maxDistance = HighPrecMeters <$> maxDistance,
            estimatedRideDuration = estimatedRideDuration,
            device = device,
            merchantId = Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            bundleVersion = bundleVersion',
            clientVersion = clientVersion',
            language = language,
            disabilityTag = disabilityTag,
            customerExtraFee = customerExtraFee,
            autoAssignEnabled = autoAssignEnabled,
            autoAssignEnabledV2 = autoAssignEnabledV2,
            availablePaymentMethods = Id <$> availablePaymentMethods,
            selectedPaymentMethodId = Id <$> selectedPaymentMethodId,
            createdAt = createdAt
          }
    where
      backfillMOCId = \case
        Just mocId -> pure $ Id mocId
        Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity (Id merchantId)

instance ToTType' BeamSR.SearchRequest SearchRequest where
  toTType' SearchRequest {..} = do
    BeamSR.SearchRequestT
      { BeamSR.id = getId id,
        BeamSR.startTime = startTime,
        BeamSR.validTill = validTill,
        BeamSR.riderId = getId riderId,
        BeamSR.fromLocationId = Just $ getId fromLocation.id,
        BeamSR.toLocationId = getId <$> (toLocation <&> (.id)),
        BeamSR.distance = getHighPrecMeters <$> distance,
        BeamSR.maxDistance = getHighPrecMeters <$> maxDistance,
        BeamSR.estimatedRideDuration = estimatedRideDuration,
        BeamSR.device = device,
        BeamSR.merchantId = getId merchantId,
        BeamSR.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        BeamSR.bundleVersion = versionToText <$> bundleVersion,
        BeamSR.clientVersion = versionToText <$> clientVersion,
        BeamSR.language = language,
        BeamSR.disabilityTag = disabilityTag,
        BeamSR.customerExtraFee = customerExtraFee,
        BeamSR.autoAssignEnabled = autoAssignEnabled,
        BeamSR.autoAssignEnabledV2 = autoAssignEnabledV2,
        BeamSR.availablePaymentMethods = getId <$> availablePaymentMethods,
        BeamSR.selectedPaymentMethodId = getId <$> selectedPaymentMethodId,
        BeamSR.createdAt = createdAt
      }

-- FUNCTIONS FOR HANDLING OLD DATA : TO BE REMOVED AFTER SOME TIME

buildLocation :: MonadFlow m => DSSL.SearchReqLocation -> m DL.Location
buildLocation DSSL.SearchReqLocation {..} = do
  return $
    DL.Location
      { id = cast id,
        ..
      }

upsertFromLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> Maybe Text -> m DL.Location
upsertFromLocationAndMappingForOldData searchRequestId merchantId merchantOperatingCityId = do
  locMapVal <- QLM.findByEntityId searchRequestId
  let fromLocationMapping = filter (\loc -> loc.order == 0) locMapVal
  fromLocMap <- listToMaybe fromLocationMapping & fromMaybeM (InternalError "Entity Mappings For FromLocation Not Found")
  fl <- QL.findById fromLocMap.locationId >>= fromMaybeM (InternalError $ "FromLocation not found in search request for fromLocationId: " <> fromLocMap.locationId.getId)
  pickupLocMapping <- SSRM.buildPickUpLocationMapping fl.id searchRequestId DSRM.SEARCH_REQUEST (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  QSRM.create pickupLocMapping
  return fl

upsertToLocationAndMappingForOldData :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> Maybe Text -> m DL.Location
upsertToLocationAndMappingForOldData searchReqId merchantId merchantOperatingCityId = do
  locMapVal <- QLM.findByEntityId searchReqId
  let toLocationMappings = filter (\loc -> loc.order /= 0) locMapVal
  toLocMap <- listToMaybe toLocationMappings & fromMaybeM (InternalError "Entity Mappings For ToLocation Not Found")
  tl <- QL.findById toLocMap.locationId >>= fromMaybeM (InternalError $ "ToLocation not found in search request for toLocationId: " <> toLocMap.locationId.getId)
  dropLocMapping <- SSRM.buildDropLocationMapping tl.id searchReqId DSRM.SEARCH_REQUEST (Just $ Id merchantId) (Id <$> merchantOperatingCityId)
  QSRM.create dropLocMapping
  return tl
