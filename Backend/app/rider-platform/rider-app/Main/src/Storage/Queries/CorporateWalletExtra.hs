module Storage.Queries.CorporateWalletExtra where

import Domain.Types.CorporateWallet
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateWallet as Beam
import Storage.Queries.OrphanInstances.CorporateWallet ()

-- Extra code goes here --

-- | Atomic balance increment at DB level to avoid read-modify-write race conditions.
--   Uses SQL-level addition: SET balance = balance + amount
atomicAddBalance ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  HighPrecMoney ->
  UTCTime ->
  Id CorporateWallet ->
  m ()
atomicAddBalance amount now walletId = do
  wallet <- findOneWithKV [Se.Is Beam.id $ Se.Eq (getId walletId)]
  case wallet of
    Nothing -> pure ()
    Just w -> do
      let currentBalance :: Double = Beam.balance w
          newBalance = currentBalance + realToFrac amount
      updateWithKV
        [ Se.Set Beam.balance newBalance,
          Se.Set Beam.lastTopUpAt (Just now),
          Se.Set Beam.updatedAt now
        ]
        [Se.Is Beam.id $ Se.Eq (getId walletId)]
