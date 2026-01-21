{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPassExtra where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Extra.PurchasedPass ()
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PassType as DPassType
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPass as Beam
import Storage.Queries.OrphanInstances.PurchasedPass ()
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  m (Maybe DPurchasedPass.PurchasedPass)
findById purchasedPassId = findOneWithKV [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

findAllByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  m [DPurchasedPass.PurchasedPass]
findAllByPersonId personId = do
  findAllWithKV
    [Se.Is Beam.personId $ Se.Eq (getId personId)]

findAllByPersonIdWithFilters ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Maybe [DPurchasedPass.StatusType] ->
  Maybe Int ->
  Maybe Int ->
  m [DPurchasedPass.PurchasedPass]
findAllByPersonIdWithFilters personId merchantId mbStatus mbLimit mbOffset = do
  let baseConds = [Se.Is Beam.personId $ Se.Eq (getId personId), Se.Is Beam.merchantId $ Se.Eq (getId merchantId)]
      statusConds = case mbStatus of
        Nothing -> []
        Just s -> [Se.Is Beam.status $ Se.In s]
      conds = baseConds ++ statusConds
  findAllWithOptionsKV conds (Se.Desc Beam.createdAt) mbLimit mbOffset

findPassByPersonIdAndPassTypeIdAndDeviceId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DPassType.PassType ->
  Text ->
  m (Maybe DPurchasedPass.PurchasedPass)
findPassByPersonIdAndPassTypeIdAndDeviceId personId merchantId passTypeId deviceId =
  do
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.personId $ Se.Eq (getId personId),
            Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
            Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
            Se.Is Beam.deviceId $ Se.Eq deviceId
          ]
      ]
      (Se.Desc Beam.createdAt)
      (Just 1)
      (Just 0)
    <&> listToMaybe

findPendingPassByPersonIdAndPassTypeId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DPassType.PassType ->
  m (Maybe DPurchasedPass.PurchasedPass)
findPendingPassByPersonIdAndPassTypeId personId merchantId passTypeId =
  findOneWithKV
    [ Se.Is Beam.personId $ Se.Eq (getId personId),
      Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
      Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
      Se.Or
        [ Se.Is Beam.status $ Se.Not $ Se.In [DPurchasedPass.Active, DPurchasedPass.PreBooked],
          Se.Or
            [ Se.Is Beam.deviceSwitchCount $ Se.LessThanOrEq (Just 1),
              Se.Is Beam.deviceSwitchCount $ Se.Eq Nothing
            ]
        ]
    ]

updatePurchaseData ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  Day ->
  Day ->
  DPurchasedPass.StatusType ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe DPurchasedPass.BenefitType ->
  Kernel.Prelude.Maybe HighPrecMoney ->
  HighPrecMoney ->
  m ()
updatePurchaseData purchasedPassId startDate endDate status benefitDescription mbBenefitType mbBenefitValue amount = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set Beam.startDate startDate,
        Se.Set Beam.endDate endDate,
        Se.Set Beam.status status,
        Se.Set Beam.usedTripCount (Just 0),
        Se.Set Beam.benefitDescription benefitDescription,
        Se.Set Beam.benefitType mbBenefitType,
        Se.Set Beam.benefitValue mbBenefitValue,
        Se.Set Beam.passAmount amount,
        Se.Set Beam.updatedAt now
      ]
        <> (if status == DPurchasedPass.Active then [Se.Set Beam.deviceSwitchCount (Just 0)] else [])
    )
    [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

updateStatusById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DPurchasedPass.StatusType ->
  Id DPurchasedPass.PurchasedPass ->
  m ()
updateStatusById status purchasedPassId = do
  now <- getCurrentTime
  updateWithKV
    ( [ Se.Set Beam.status status,
        Se.Set Beam.updatedAt now
      ]
        <> (if status == DPurchasedPass.Active then [Se.Set Beam.deviceSwitchCount (Just 0)] else [])
    )
    [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

updateDeviceIdById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Int ->
  Id DPurchasedPass.PurchasedPass ->
  m ()
updateDeviceIdById deviceId deviceSwitchCount purchasedPassId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.deviceId deviceId,
      Se.Set Beam.deviceSwitchCount (Just deviceSwitchCount),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

updateProfilePictureById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Id DPurchasedPass.PurchasedPass ->
  m ()
updateProfilePictureById mbProfilePicture purchasedPassId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.profilePicture mbProfilePicture,
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

getLastPassNumber ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m Int
getLastPassNumber = do
  pass <- findAllWithOptionsDb [Se.Is Beam.id $ Se.Not $ Se.Eq ""] (Se.Desc Beam.passNumber) (Just 1) (Just 0)
  return $ case listToMaybe pass of
    Just p -> p.passNumber
    Nothing -> 0

deleteById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DPurchasedPass.PurchasedPass ->
  m ()
deleteById purchasedPassId = deleteWithKV [Se.Is Beam.id $ Se.Eq (getId purchasedPassId)]

findAllByPersonIdAndPassTypeIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Id DPassType.PassType ->
  [DPurchasedPass.StatusType] ->
  m [DPurchasedPass.PurchasedPass]
findAllByPersonIdAndPassTypeIdAndStatus personId merchantId passTypeId statuses =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (getId personId),
          Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
          Se.Is Beam.passTypeId $ Se.Eq (getId passTypeId),
          Se.Is Beam.status $ Se.In statuses
        ]
    ]

findAllActiveByPersonIdWithFiltersV2 ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Id DM.Merchant ->
  Maybe [Id DPassType.PassType] ->
  Maybe Day ->
  Maybe Day ->
  Maybe Int ->
  Maybe Int ->
  m [DPurchasedPass.PurchasedPass]
findAllActiveByPersonIdWithFiltersV2 personId merchantId mbPassTypeIds mbFromDate mbToDate mbLimit mbOffset = do
  let baseConds = [Se.Is Beam.personId $ Se.Eq (getId personId), Se.Is Beam.merchantId $ Se.Eq (getId merchantId), Se.Is Beam.status $ Se.Eq DPurchasedPass.Active]
  let passTypeConds =
        case mbPassTypeIds of
          Just ids | not (null ids) -> [Se.Is Beam.passTypeId $ Se.In (map getId ids)]
          _ -> []
      fromDateCond = maybe [] (\d -> [Se.Is Beam.startDate $ Se.GreaterThanOrEq d]) mbFromDate
      toDateCond = maybe [] (\d -> [Se.Is Beam.startDate $ Se.LessThanOrEq d]) mbToDate
      conds = baseConds ++ passTypeConds ++ fromDateCond ++ toDateCond

  findAllWithOptionsKV conds (Se.Desc Beam.createdAt) mbLimit mbOffset
