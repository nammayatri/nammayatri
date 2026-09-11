module Storage.Queries.SearchRequestExtra where

import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Domain.Utils (mapConcurrently)
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.SearchRequest ()

createDSReq' :: (MonadFlow m, EsqDBFlow m r) => SearchRequest -> m ()
createDSReq' searchReq = do
  if fromMaybe False searchReq.isMultimodalSearch then createWithKVWithOptions (Just 21600) True searchReq else createWithKV searchReq

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SearchRequest -> m ()
create dsReq = do
  _ <- whenNothingM_ (QL.findById dsReq.fromLocation.id) $ do QL.create dsReq.fromLocation
  _ <- whenJust dsReq.toLocation $ \location -> processLocation location
  createDSReq' dsReq
  where
    processLocation location = whenNothingM_ (QL.findById location.id) $ do QL.create location

createStopsLocation :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Forkable m) => [DL.Location] -> m ()
createStopsLocation locations = void $ mapConcurrently QL.create locations

-- | Persists the pickup/stops/drop LocationMapping rows for a SearchRequest that is
-- being created right now, so its id (and thus every mapping's entityId) is brand new.
-- Uses the "fresh" builders (see SharedLogic.LocationMapping) rather than
-- buildPickUpLocationMapping/buildStopsLocationMapping/buildDropLocationMapping: those
-- also query for past versions of the mapping to supersede, which a brand-new entityId
-- can never have, and the drop's order is computed from the stop count directly instead
-- of via a query, for the same reason. The stop and mapping writes that remain run
-- concurrently rather than one at a time.
createDSReq :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Forkable m) => SearchRequest -> m ()
createDSReq searchRequest = do
  fromLocationMap <- SLM.buildFreshLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId) 0
  void $ QLM.create fromLocationMap
  void $ createStopsLocation searchRequest.stops
  stopsLocMapping <- SLM.buildFreshStopsLocationMapping searchRequest.stops searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId)
  void $ mapConcurrently QLM.create stopsLocMapping
  mbToLocationMap <- maybe (pure Nothing) (\detail -> Just <$> SLM.buildFreshLocationMapping detail.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId) (length searchRequest.stops + 1)) searchRequest.toLocation
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap
  create searchRequest

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

findAllByPerson :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [SearchRequest]
findAllByPerson (Id personId) = findAllWithKV [Se.Is BeamSR.riderId $ Se.Eq personId]

-- | Marks a parent as having a walk-and-save suggestion, so the readers can tell without
-- going looking. One write on the uncommon path in exchange for no read on the common one.
updateHasBetterPointSuggestion :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> m ()
updateHasBetterPointSuggestion (Id searchRequestId) =
  updateOneWithKV
    [Se.Set BeamSR.hasBetterPointSuggestion (Just True)]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

findLatestSearchRequest :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe SearchRequest)
findLatestSearchRequest (Id riderId) = findAllWithOptionsKV [Se.Is BeamSR.riderId $ Se.Eq riderId] (Se.Desc BeamSR.createdAt) (Just 1) Nothing <&> listToMaybe

findLastSearchRequestInKV :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m (Maybe SearchRequest)
findLastSearchRequestInKV (Id riderId) = findAllFromKvRedis [Se.Is BeamSR.riderId $ Se.Eq riderId] (Just $ Se.Desc BeamSR.createdAt) <&> listToMaybe

updateCustomerExtraFeeAndPaymentMethod :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> Maybe Price -> Maybe Payment.PaymentMethodId -> Maybe DMPM.PaymentInstrument -> m ()
updateCustomerExtraFeeAndPaymentMethod (Id searchReqId) customerExtraFee paymentMethodId paymentInstrument =
  updateOneWithKV
    [ Se.Set BeamSR.customerExtraFee $ customerExtraFee <&> (.amountInt),
      Se.Set BeamSR.customerExtraFeeAmount $ customerExtraFee <&> (.amount),
      Se.Set BeamSR.currency $ customerExtraFee <&> (.currency),
      Se.Set BeamSR.selectedPaymentMethodId paymentMethodId,
      Se.Set BeamSR.selectedPaymentInstrument paymentInstrument
    ]
    [Se.Is BeamSR.id (Se.Eq searchReqId)]

updateAutoAssign :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> Bool -> Bool -> m ()
updateAutoAssign (Id searchRequestId) autoAssignedEnabled autoAssignedEnabledV2 = do
  updateOneWithKV
    [ Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled,
      Se.Set BeamSR.autoAssignEnabledV2 $ Just autoAssignedEnabledV2
    ]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

updateMultipleByRequestId :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> Bool -> Bool -> Maybe Bool -> m ()
updateMultipleByRequestId (Id searchRequestId) autoAssignedEnabled autoAssignedEnabledV2 isAdvanceBookingEnabled = do
  updateOneWithKV
    [ Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled,
      Se.Set BeamSR.autoAssignEnabledV2 $ Just autoAssignedEnabledV2,
      Se.Set BeamSR.isAdvanceBookingEnabled isAdvanceBookingEnabled
    ]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

updateDisability :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> Maybe Text -> m ()
updateDisability (Id searchRequestId) disability = do
  updateOneWithKV
    [Se.Set BeamSR.disabilityTag disability]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

findAllById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Text] -> m [SearchRequest]
findAllById srids = findAllWithKV [Se.Is BeamSR.id $ Se.In srids]

updateStartTime :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> UTCTime -> m ()
updateStartTime (Id searchRequestId) startTime = do
  updateOneWithKV
    [Se.Set BeamSR.startTime startTime]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

updateOffersFraudCheckFailureReason :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> Text -> m ()
updateOffersFraudCheckFailureReason (Id searchRequestId) failureReason = do
  updateOneWithKV
    [Se.Set BeamSR.offersFraudCheckFailureReason (Just failureReason)]
    [ Se.Is BeamSR.id (Se.Eq searchRequestId),
      Se.Is BeamSR.offersFraudCheckFailureReason (Se.Eq Nothing)
    ]

updateFromSpecialLocationId :: (MonadFlow m, EsqDBFlow m r) => Id SearchRequest -> Maybe Text -> m ()
updateFromSpecialLocationId (Id searchRequestId) fromSpecialLocationId =
  updateOneWithKV
    [Se.Set BeamSR.fromSpecialLocationId fromSpecialLocationId]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]
