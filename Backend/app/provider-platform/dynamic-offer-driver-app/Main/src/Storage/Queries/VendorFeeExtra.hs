module Storage.Queries.VendorFeeExtra where

import qualified Data.Map as M
import Domain.Types.DriverFee
import qualified Domain.Types.MerchantOperatingCity as DMC
import Domain.Types.VendorFee
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, getLocalCurrentTime, throwError)
import qualified Sequelize as Se
import SharedLogic.Payment (roundToTwoDecimalPlaces)
import qualified Storage.Beam.VendorFee as Beam
import qualified Storage.Cac.TransporterConfig as SCTC
import Storage.Queries.OrphanInstances.VendorFee ()

-- Extra code goes here --

findAllByDriverFeeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DriverFee -> m [VendorFee]
findAllByDriverFeeId driverFeeId = findAllWithKV [Se.Is Beam.driverFeeId $ Se.Eq driverFeeId.getId]

findByVendorAndDriverFeeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Id DriverFee -> m (Maybe VendorFee)
findByVendorAndDriverFeeId vendorId driverFeeId = do findOneWithKV [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]

updateVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMC.MerchantOperatingCity -> VendorFee -> m ()
updateVendorFee merchantOpCityId vendorFee = do
  oldVendorFee <- findByVendorAndDriverFeeId vendorFee.vendorId vendorFee.driverFeeId
  case oldVendorFee of
    Just fee -> do
      transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
      now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      updateWithKV
        [ Se.Set Beam.amount (roundToTwoDecimalPlaces (HighPrecMoney $ fee.amount.getHighPrecMoney + vendorFee.amount.getHighPrecMoney)),
          Se.Set Beam.updatedAt now
        ]
        [ Se.And
            [ Se.Is Beam.driverFeeId $ Se.Eq vendorFee.driverFeeId.getId,
              Se.Is Beam.vendorId $ Se.Eq vendorFee.vendorId
            ]
        ]
    _ -> createWithKV vendorFee

adjustVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DriverFee -> Rational -> m ()
adjustVendorFee driverFeeId adjustment = do
  when (adjustment <= 0) $ throwError (InternalError "Adjustment ratio must be positive")
  oldVendorFees <- findAllByDriverFeeId driverFeeId
  for_ oldVendorFees $ \fee -> do
    now <- getCurrentTime
    let newAmount = roundToTwoDecimalPlaces $ HighPrecMoney $ ((fee.amount.getHighPrecMoney) * (adjustment))
    updateWithKV
      [ Se.Set Beam.amount newAmount,
        Se.Set Beam.updatedAt now
      ]
      [ Se.And
          [ Se.Is Beam.driverFeeId $ Se.Eq driverFeeId.getId,
            Se.Is Beam.vendorId $ Se.Eq fee.vendorId
          ]
      ]

-- Adjust original vendor fee by subtracting the sum of vendor fees created for child driver fees
adjustVendorFeeSubtractingChildren :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DriverFee -> [Id DriverFee] -> m ()
adjustVendorFeeSubtractingChildren parentDriverFeeId childDriverFeeIds = do
  when (null childDriverFeeIds) $ pure ()
  oldVendorFees <- findAllByDriverFeeId parentDriverFeeId
  childVendorFees <- concat <$> mapM findAllByDriverFeeId childDriverFeeIds
  let childSums :: M.Map Text Rational
      childSums = M.fromListWith (+) [(v.vendorId, v.amount.getHighPrecMoney) | v <- childVendorFees]
  now <- getCurrentTime
  for_ oldVendorFees $ \fee -> do
    let childSum = M.findWithDefault 0 fee.vendorId childSums
        remaining = max 0 (fee.amount.getHighPrecMoney - childSum)
        newAmount = roundToTwoDecimalPlaces $ HighPrecMoney remaining
    updateWithKV
      [ Se.Set Beam.amount newAmount,
        Se.Set Beam.updatedAt now
      ]
      [ Se.And
          [ Se.Is Beam.driverFeeId $ Se.Eq parentDriverFeeId.getId,
            Se.Is Beam.vendorId $ Se.Eq fee.vendorId
          ]
      ]

createChildVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DriverFee -> DriverFee -> HighPrecMoney -> m ()
createChildVendorFee parentFee childFee totalFee = do
  now <- getCurrentTime
  let adjustment =
        if (getHighPrecMoney totalFee) == 0
          then 1.0
          else (getHighPrecMoney childFee.platformFee.fee + getHighPrecMoney childFee.platformFee.cgst + getHighPrecMoney childFee.platformFee.sgst) / (getHighPrecMoney totalFee)

  vendorFees <- findAllByDriverFeeId parentFee.id
  let childVendorFees =
        map
          ( \vfee ->
              VendorFee
                { amount = roundToTwoDecimalPlaces $ HighPrecMoney $ (vfee.amount.getHighPrecMoney * adjustment),
                  driverFeeId = childFee.id,
                  vendorId = vfee.vendorId,
                  createdAt = now,
                  updatedAt = now
                }
          )
          vendorFees
  traverse_ createWithKV childVendorFees
  pure ()

updateManyVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMC.MerchantOperatingCity -> [VendorFee] -> m ()
updateManyVendorFee merchantOpCityId = traverse_ $ updateVendorFee merchantOpCityId

resetVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMC.MerchantOperatingCity -> [VendorFee] -> m ()
resetVendorFee merchantOpCityId vendorFees = do
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  forM_ vendorFees $ \vendorFee -> do
    updateWithKV
      [ Se.Set Beam.amount (roundToTwoDecimalPlaces (HighPrecMoney $ vendorFee.amount.getHighPrecMoney)),
        Se.Set Beam.updatedAt now
      ]
      [ Se.And
          [ Se.Is Beam.driverFeeId $ Se.Eq vendorFee.driverFeeId.getId,
            Se.Is Beam.vendorId $ Se.Eq vendorFee.vendorId
          ]
      ]

-- Updates existing vendor fee by adding new amount, respecting maxVendorFeeAmount limit
-- If oldVendorFeeAmount == limit: skip update. If oldVendorFeeAmount + newAmount > limit: cap at limit. Otherwise: add normally
updateVendorFeeWithMaxLimit :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMC.MerchantOperatingCity -> VendorFee -> Maybe HighPrecMoney -> m ()
updateVendorFeeWithMaxLimit merchantOpCityId vendorFee maxLimit = do
  mbOldVendorFee <- findByVendorAndDriverFeeId vendorFee.vendorId vendorFee.driverFeeId
  case mbOldVendorFee of
    Just oldVendorFee -> do
      let shouldUpdate = maybe True (> oldVendorFee.amount) maxLimit
      when shouldUpdate $ do
        let newTotal = HighPrecMoney $ oldVendorFee.amount.getHighPrecMoney + vendorFee.amount.getHighPrecMoney
            finalAmount = maybe newTotal (min newTotal) maxLimit
        transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
        now <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        updateWithKV
          [ Se.Set Beam.amount (roundToTwoDecimalPlaces finalAmount),
            Se.Set Beam.updatedAt now
          ]
          [ Se.And
              [ Se.Is Beam.driverFeeId $ Se.Eq vendorFee.driverFeeId.getId,
                Se.Is Beam.vendorId $ Se.Eq vendorFee.vendorId
              ]
          ]
    _ -> createVendorFeeWithMaxLimit vendorFee maxLimit

-- Creates new vendor fee, capping amount at maxVendorFeeAmount if it exceeds the limit
createVendorFeeWithMaxLimit :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => VendorFee -> Maybe HighPrecMoney -> m ()
createVendorFeeWithMaxLimit VendorFee {..} maxLimit = do
  let finalAmount = maybe amount (min amount) maxLimit
  createWithKV VendorFee {amount = finalAmount, ..}

updateManyVendorFeeWithMaxLimit :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DMC.MerchantOperatingCity -> [(VendorFee, Maybe HighPrecMoney)] -> m ()
updateManyVendorFeeWithMaxLimit merchantOpCityId = traverse_ $ \(vendorFee, maxLimit) -> updateVendorFeeWithMaxLimit merchantOpCityId vendorFee maxLimit

createManyWithMaxLimit :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [(VendorFee, Maybe HighPrecMoney)] -> m ()
createManyWithMaxLimit = traverse_ $ \(vendorFee, maxLimit) -> createVendorFeeWithMaxLimit vendorFee maxLimit
