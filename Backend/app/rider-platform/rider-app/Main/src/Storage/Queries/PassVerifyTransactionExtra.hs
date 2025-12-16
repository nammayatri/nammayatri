{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PassVerifyTransactionExtra where

import qualified Domain.Types.PurchasedPass as PurchasedPass
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PassVerifyTransaction as Beam
import qualified Storage.Queries.OrphanInstances.PassVerifyTransaction ()

-- Extra code goes here --
findLastVerifiedVehicleNumberByPurchasePassId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id PurchasedPass.PurchasedPass ->
  m (Maybe (Text, Maybe Kernel.Prelude.Bool))
findLastVerifiedVehicleNumberByPurchasePassId purchasePassId = do
  now <- getCurrentTime
  transactions <-
    findAllWithOptionsKV
      [ Se.And
          [ Se.Is Beam.purchasePassId $ Se.Eq (getId purchasePassId),
            Se.Is Beam.validTill $ Se.GreaterThan now
          ]
      ]
      (Se.Desc Beam.verifiedAt)
      (Just 1)
      (Just 0)
  case listToMaybe transactions of
    Just transaction -> return (Just (transaction.fleetId, transaction.autoActivated))
    Nothing -> return Nothing
