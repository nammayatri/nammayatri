{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Keeps 'DriverPoolData.walletBalance' in step with the wallet ledger, so the pool can check
--   the go-online minimum without reading finance_account. Called from
--   'SharedLogic.Finance.PostActions' after every posting that moved a wallet balance.
module SharedLogic.Finance.WalletBalanceCache
  ( syncWalletBalanceToPool,
    setDriverWalletBalanceInPool,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.Account as FAccount
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.Account as QFAccount
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync

-- | Write a driver wallet's current balance into that driver's pool data. The pool always checks the
--   driver's own wallet (the charges of this feature settle there, fleet driver or not), so a posting on
--   a fleet owner's wallet changes nothing here.
--   A cache that ends up stale is repaired by the next posting, the go-online check or the integrity job.
--
--   Called only for cities with the wallet on (PostActions reads the city's wallet config): the
--   pool reads this balance only where the minimum is enforced, which needs the wallet on (and keeping it
--   current there means it is already right when enforcement is switched on).
syncWalletBalanceToPool ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id FAccount.Account ->
  m ()
syncWalletBalanceToPool accountId = do
  mbAccount <- QFAccount.findById accountId
  whenJust mbAccount $ \account ->
    when (account.accountType == FAccount.Liability && isNothing account.subLedger) $
      case (account.counterpartyType, account.counterpartyId) of
        (Just FAccount.DRIVER, Just driverId) -> setDriverWalletBalanceInPool (Id driverId) account.balance
        _ -> pure ()

setDriverWalletBalanceInPool ::
  (MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  HighPrecMoney ->
  m ()
setDriverWalletBalanceInPool driverId balance =
  LTSSync.syncDriverPoolDataToLTS (cast driverId) $
    LTSSync.emptyUpdate {LTSSync.walletBalance = LTSSync.Set (Just balance)}
