module Storage.Queries.VendorFeeExtra where

import Domain.Types.DriverFee
import Domain.Types.VendorFee
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.Price
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime, throwError)
import qualified Sequelize as Se
import SharedLogic.Payment (roundToTwoDecimalPlaces)
import qualified Storage.Beam.VendorFee as Beam
import Storage.Queries.OrphanInstances.VendorFee ()

-- Extra code goes here --

findAllByDriverFeeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DriverFee -> m [VendorFee]
findAllByDriverFeeId driverFeeId = findAllWithKV [Se.Is Beam.driverFeeId $ Se.Eq driverFeeId.getId]

findByVendorAndDriverFeeId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Id DriverFee -> m (Maybe VendorFee)
findByVendorAndDriverFeeId vendorId driverFeeId = do findOneWithKV [Se.And [Se.Is Beam.driverFeeId $ Se.Eq (Kernel.Types.Id.getId driverFeeId), Se.Is Beam.vendorId $ Se.Eq vendorId]]

updateVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => VendorFee -> m ()
updateVendorFee vendorFee = do
  oldVendorFee <- findByVendorAndDriverFeeId vendorFee.vendorId vendorFee.driverFeeId
  case oldVendorFee of
    Just fee -> do
      now <- getCurrentTime
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

createChildVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DriverFee -> DriverFee -> m ()
createChildVendorFee parentFee childFee = do
  now <- getCurrentTime
  let adjustment =
        if (getHighPrecMoney parentFee.platformFee.fee) == 0
          then 1.0
          else (getHighPrecMoney childFee.platformFee.fee) / (getHighPrecMoney parentFee.platformFee.fee)

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

updateManyVendorFee :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [VendorFee] -> m ()
updateManyVendorFee = traverse_ updateVendorFee
