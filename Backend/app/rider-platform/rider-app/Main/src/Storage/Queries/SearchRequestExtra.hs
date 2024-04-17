{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequestExtra where

import Data.Text (strip)
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.MerchantPaymentMethod (MerchantPaymentMethod)
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import EulerHS.Prelude (whenNothingM_)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.Beam.SearchRequest as BeamSR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import Storage.Queries.OrphanInstances.SearchRequest
import Tools.Error

createDSReq' :: KvDbFlow m r => SearchRequest -> m ()
createDSReq' = createWithKV

create :: KvDbFlow m r => SearchRequest -> m ()
create dsReq = do
  _ <- whenNothingM_ (QL.findById dsReq.fromLocation.id) $ do QL.create dsReq.fromLocation
  _ <- whenJust dsReq.toLocation $ \location -> processLocation location
  createDSReq' dsReq
  where
    processLocation location = whenNothingM_ (QL.findById location.id) $ do QL.create location

createDSReq :: KvDbFlow m r => SearchRequest -> m ()
createDSReq searchRequest = do
  fromLocationMap <- SLM.buildPickUpLocationMapping searchRequest.fromLocation.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId)
  mbToLocationMap <- maybe (pure Nothing) (\detail -> Just <$> SLM.buildDropLocationMapping detail.id searchRequest.id.getId DLM.SEARCH_REQUEST (Just searchRequest.merchantId) (Just searchRequest.merchantOperatingCityId)) searchRequest.toLocation
  void $ QLM.create fromLocationMap
  void $ whenJust mbToLocationMap $ \toLocMap -> QLM.create toLocMap
  create searchRequest

findById :: KvDbFlow m r => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

findByPersonId :: KvDbFlow m r => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId (Id personId) (Id searchRequestId) = findOneWithKV [Se.And [Se.Is BeamSR.id $ Se.Eq searchRequestId, Se.Is BeamSR.riderId $ Se.Eq personId]]

findAllByPerson :: KvDbFlow m r => Id Person -> m [SearchRequest]
findAllByPerson (Id personId) = findAllWithKV [Se.Is BeamSR.riderId $ Se.Eq personId]

findLatestSearchRequest :: KvDbFlow m r => Id Person -> m (Maybe SearchRequest)
findLatestSearchRequest (Id riderId) = findAllWithOptionsKV [Se.Is BeamSR.riderId $ Se.Eq riderId] (Se.Desc BeamSR.createdAt) (Just 1) Nothing <&> listToMaybe

updateCustomerExtraFeeAndPaymentMethod :: KvDbFlow m r => Id SearchRequest -> Maybe Price -> Maybe (Id DMPM.MerchantPaymentMethod) -> m ()
updateCustomerExtraFeeAndPaymentMethod (Id searchReqId) customerExtraFee paymentMethodId =
  updateOneWithKV
    [ Se.Set BeamSR.customerExtraFee $ customerExtraFee <&> (.amountInt),
      Se.Set BeamSR.customerExtraFeeAmount $ customerExtraFee <&> (.amount),
      Se.Set BeamSR.currency $ customerExtraFee <&> (.currency),
      Se.Set BeamSR.selectedPaymentMethodId (getId <$> paymentMethodId)
    ]
    [Se.Is BeamSR.id (Se.Eq searchReqId)]

updateAutoAssign :: KvDbFlow m r => Id SearchRequest -> Bool -> Bool -> m ()
updateAutoAssign (Id searchRequestId) autoAssignedEnabled autoAssignedEnabledV2 = do
  updateOneWithKV
    [ Se.Set BeamSR.autoAssignEnabled $ Just autoAssignedEnabled,
      Se.Set BeamSR.autoAssignEnabledV2 $ Just autoAssignedEnabledV2
    ]
    [Se.Is BeamSR.id (Se.Eq searchRequestId)]

updatePaymentMethods :: KvDbFlow m r => Id SearchRequest -> [Id MerchantPaymentMethod] -> m ()
updatePaymentMethods (Id searchReqId) availablePaymentMethods =
  updateOneWithKV
    [ Se.Set BeamSR.availablePaymentMethods (getId <$> availablePaymentMethods)
    ]
    [Se.Is BeamSR.id (Se.Eq searchReqId)]
