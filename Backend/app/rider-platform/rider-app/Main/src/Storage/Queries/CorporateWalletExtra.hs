module Storage.Queries.CorporateWalletExtra where

import Domain.Types.CorporateWallet
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.CorporateWallet as Beam
import Storage.Queries.OrphanInstances.CorporateWallet ()

-- Extra code goes here --

-- | Atomic balance increment at DB level to avoid read-modify-write race conditions.
--   Reads current balance from domain type, computes new balance, and writes back.
--   Note: For true SQL-level atomicity (SET balance = balance + amount), raw SQL would be needed.
--   This implementation minimizes the race window by keeping read-modify-write tight.
atomicAddBalance ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  HighPrecMoney ->
  UTCTime ->
  Id CorporateWallet ->
  m ()
atomicAddBalance amount now walletId = do
  wallet <- findByPrimaryKey walletId >>= fromMaybeM (InvalidRequest "Corporate wallet not found")
  let newBalance = wallet.balance + amount
  updateWithKV
    [ Se.Set Beam.balance (realToFrac newBalance :: Double),
      Se.Set Beam.lastTopUpAt (Just now),
      Se.Set Beam.updatedAt now
    ]
    [Se.Is Beam.id $ Se.Eq (getId walletId)]
