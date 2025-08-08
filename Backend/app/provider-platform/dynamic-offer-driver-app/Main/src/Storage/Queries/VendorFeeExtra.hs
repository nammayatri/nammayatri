module Storage.Queries.VendorFeeExtra where

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

createChildVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DriverFee -> DriverFee -> HighPrecMoney -> m ()
createChildVendorFee parentFee childFee totalFee = do
  now <- getCurrentTime
  let adjustment =
        if (getHighPrecMoney totalFee) == 0
          then 1.0
          else (getHighPrecMoney childFee.platformFee.fee) / (getHighPrecMoney totalFee)

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
