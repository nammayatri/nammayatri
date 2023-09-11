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

import Domain.Types.Merchant.MerchantPaymentMethod (MerchantPaymentMethod)
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import Domain.Types.Person (Person)
import Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Version
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as BeamSR
import Storage.Queries.SearchRequest.SearchReqLocation as QSRL

createDSReq :: MonadFlow m => SearchRequest -> m ()
createDSReq = createWithKV

create :: MonadFlow m => SearchRequest -> m ()
create dsReq = do
  _ <- QSRL.create dsReq.fromLocation
  _ <- traverse_ QSRL.create dsReq.toLocation
  createDSReq dsReq

findById :: MonadFlow m => Id SearchRequest -> m (Maybe SearchRequest)
findById (Id searchRequestId) = findOneWithKV [Se.Is BeamSR.id $ Se.Eq searchRequestId]

findByPersonId :: MonadFlow m => Id Person -> Id SearchRequest -> m (Maybe SearchRequest)
findByPersonId (Id personId) (Id searchRequestId) = findOneWithKV [Se.And [Se.Is BeamSR.id $ Se.Eq searchRequestId, Se.Is BeamSR.riderId $ Se.Eq personId]]

findAllByPerson :: MonadFlow m => Id Person -> m [SearchRequest]
findAllByPerson (Id personId) = findAllWithKV [Se.Is BeamSR.riderId $ Se.Eq personId]

findLatestSearchRequest :: MonadFlow m => Id Person -> m (Maybe SearchRequest)
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
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    fl <- QSRL.findById (Id fromLocationId) >>= fromMaybeM (InternalError $ "FromLocation not found in SearchRequest for fromLocationId:" <> show fromLocationId)
    tl <- maybe (pure Nothing) (QSRL.findById . Id) toLocationId
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

instance ToTType' BeamSR.SearchRequest SearchRequest where
  toTType' SearchRequest {..} = do
    BeamSR.SearchRequestT
      { BeamSR.id = getId id,
        BeamSR.startTime = startTime,
        BeamSR.validTill = validTill,
        BeamSR.riderId = getId riderId,
        BeamSR.fromLocationId = getId fromLocation.id,
        BeamSR.toLocationId = getId <$> (toLocation <&> (.id)),
        BeamSR.distance = getHighPrecMeters <$> distance,
        BeamSR.maxDistance = getHighPrecMeters <$> maxDistance,
        BeamSR.estimatedRideDuration = estimatedRideDuration,
        BeamSR.device = device,
        BeamSR.merchantId = getId merchantId,
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
