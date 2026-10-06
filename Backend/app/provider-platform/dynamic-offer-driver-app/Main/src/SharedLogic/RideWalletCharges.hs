{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Composes the wallet-settled charges a ride puts on the driver.
--
--   Each charge lives in its own module and owns both halves of itself — how much is owed, and
--   how it posts to the ledger:
--
--     * 'SharedLogic.AirportEntryFee'   — airport entry fee + gate DriverFeeItems
--     * 'SharedLogic.PlatformFeeCharge' — wallet-settled platform fee
--
--   This module knows neither. It collects them into a list and drives two things off it:
--
--     1. ONE balance check before the ride. Checking charges separately lets a driver who can
--        afford each one but not their total pass every check and finish with a negative wallet.
--     2. ONE ledger block at EndRide, so the postings are atomic and ordered.
--
--   TO ADD A NEW CHARGE: write its module, expose a function returning a 'WalletCharge', and add
--   one line to 'chargesForBooking' and 'chargesForRide'. Nothing else here changes.
module SharedLogic.RideWalletCharges
  ( checkWalletBalanceBeforeRide,
    chargeWalletAtRideStart,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FareParameters as DFare
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, fromEitherM, logInfo, throwError)
import Lib.Finance (CounterpartyType (DRIVER))
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified SharedLogic.AirportEntryFee as AirportEntryFee
import SharedLogic.Finance.PostActions (runFinance)
import qualified SharedLogic.Finance.Wallet as Wallet
import SharedLogic.Finance.WalletCharge (WalletCharge (..), requiredBalance, totalDebit)
import qualified SharedLogic.PlatformFeeCharge as PlatformFeeCharge
import Tools.Error

type ChargeFlow m r =
  ( Esq.EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    MonadFlow m,
    CacheFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r
  )

-- | Every wallet-settled charge a ride carries.
--
--   ADD NEW CHARGES HERE — this is the only list. The balance check and the ledger posting are
--   both driven off it, so a charge added here is automatically checked and debited.
--
--   @fareParams@ is what the charge is priced from: the booking's before the ride, the recomputed
--   ride-end ones at EndRide.
walletCharges ::
  ChargeFlow m r =>
  DTConf.TransporterConfig ->
  DI.DriverInformation ->
  Id DP.Person ->
  DFare.FareParameters ->
  SRB.Booking ->
  m [WalletCharge m]
walletCharges transporterConfig driverInfo driverId fareParams booking =
  sequence
    [ AirportEntryFee.airportWalletCharge (fromMaybe False transporterConfig.airportEntryFeeEnabled) transporterConfig booking,
      PlatformFeeCharge.platformFeeWalletCharge transporterConfig driverInfo driverId fareParams
    ]

-- | One balance check covering every wallet-settled charge the ride will debit, run before the
--   ride starts. Does nothing when there is nothing to charge. A missing wallet account counts as
--   zero balance.
--
--   Runs unconditionally. @airportEntryFeeCheckAtStartRide@ still decides whether the driver pool
--   pre-filters on airport balance at search time, but it no longer suppresses this check: a ride
--   can carry charges the pool never filtered on (the platform fee), and a balance that cleared at
--   search time can be spent before the ride starts.
checkWalletBalanceBeforeRide ::
  ChargeFlow m r =>
  DTConf.TransporterConfig ->
  DI.DriverInformation ->
  Id DP.Person ->
  SRB.Booking ->
  m ()
checkWalletBalanceBeforeRide transporterConfig driverInfo driverId booking =
  walletCharges transporterConfig driverInfo driverId booking.fareParams booking >>= ensureSufficientBalance driverId

ensureSufficientBalance ::
  (BeamFlow m r, MonadFlow m) =>
  Id DP.Person ->
  [WalletCharge m] ->
  m ()
ensureSufficientBalance driverId charges =
  whenJust (requiredBalance charges) $ \required -> do
    mbAccount <- Wallet.getWalletAccountByOwner DRIVER driverId.getId
    let available = maybe 0 (.balance) mbAccount
    when (available < required) $ do
      logInfo $
        "wallet charges: insufficient balance for " <> show (map (.label) charges)
          <> ", required: "
          <> show required
          <> ", available: "
          <> show available
      throwError $ InsufficientAirportBalance required available

-- | Check the wallet covers every charge, then post them all in ONE ledger block. Called at ride
--   start.
--
--   Check and charge share one 'walletCharges' list on purpose: built twice they would be two sets
--   of gate and config lookups, and could disagree if anything changed between them — leaving us
--   charging for something we never checked, or refusing over something we never take.
--
--   Throws 'InsufficientAirportBalance' rather than letting the wallet go negative, so call it
--   while refusing is still free: after the ride's own validation has passed, but before the ride
--   is actually started.
--
--   Charges post in list order, which is deliberate: third-party money (the airport operator)
--   before platform revenue, so a driver who cannot cover everything ends up short on the charge
--   we own rather than on money owed to someone else.
--
--   Prices from the BOOKING's fare params — correct for both charges here, since the flat
--   'WalletCharged' platform fee and the gate-configured airport charges are neither
--   distance- nor duration-dependent.
chargeWalletAtRideStart ::
  ( ChargeFlow m r,
    EncFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DTConf.TransporterConfig ->
  DI.DriverInformation ->
  DRide.Ride ->
  SRB.Booking ->
  m ()
chargeWalletAtRideStart transporterConfig driverInfo ride booking = do
  charges <- walletCharges transporterConfig driverInfo ride.driverId booking.fareParams booking
  ensureSufficientBalance ride.driverId charges
  unless (totalDebit charges <= 0) $ do
    isOnline <- Wallet.resolveIsOnlineFromBooking booking
    ctx <- Wallet.financeCtxFromRide transporterConfig booking ride Nothing isOnline
    result <- runFinance ctx $ traverse_ (.postLegs) charges
    case result of
      Left err -> fromEitherM (\e -> InternalError ("Ride wallet charge at ride start failed: " <> show e)) (Left err)
      Right _ -> pure ()
