{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The shape every wallet-settled ride charge takes, so they can be checked and posted together.
--
--   Adding a new charge to a ride means:
--
--     1. write its module — it owns how much is owed and how it posts to the ledger;
--     2. expose one function returning a 'WalletCharge';
--     3. add it to the list in 'SharedLogic.RideWalletCharges'.
--
--   Nothing else changes: the balance check and the ledger posting are driven off the list.
module SharedLogic.Finance.WalletCharge
  ( WalletCharge (..),
    totalDebit,
    requiredBalance,
  )
where

import Kernel.Prelude
import Kernel.Types.Common
import Lib.Finance (FinanceM)

-- | One wallet-settled charge on a ride.
--
--   'debitAmount' and 'minBalanceFloor' are deliberately separate, because they combine
--   differently and mixing them up under-charges or over-blocks:
--
--     * 'debitAmount' is money that will actually leave the wallet. Costs SUM.
--     * 'minBalanceFloor' is a policy minimum that is never debited (e.g. a gate's
--       @minBalanceRequired@, "hold at least this much to take such a ride"). Floors MAX.
--
--   'postLegs' runs inside a caller-supplied FinanceM block, so every charge on a ride lands in
--   one atomic posting. A charge must not open its own 'runFinance'.
data WalletCharge m = WalletCharge
  { -- | For logs and error messages; not shown to drivers.
    label :: Text,
    debitAmount :: HighPrecMoney,
    minBalanceFloor :: Maybe HighPrecMoney,
    postLegs :: FinanceM m ()
  }

-- | What the ride will actually debit across every charge.
totalDebit :: [WalletCharge m] -> HighPrecMoney
totalDebit = sum . map (.debitAmount)

-- | The balance the wallet must hold before the ride: every cost summed, raised to the highest
--   policy floor when that is higher. 'Nothing' when there is nothing to require.
requiredBalance :: [WalletCharge m] -> Maybe HighPrecMoney
requiredBalance charges =
  let highestFloor = maximum (0 : mapMaybe (.minBalanceFloor) charges)
      required = max highestFloor (totalDebit charges)
   in if required > 0 then Just required else Nothing
