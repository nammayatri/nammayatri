{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Wallet
  ( createWallet,
    walletPosting,
    walletReversal,
    walletBalance,
    walletVerifyTxn,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Wallet.Interface as Wallet
import qualified Kernel.External.Wallet.Juspay.Flow as Juspay
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC

createWallet ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Wallet.CreateWalletReq ->
  m Wallet.CreateWalletResp
createWallet = runWithServiceConfig Juspay.createWallet

walletPosting ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Wallet.WalletPostingReq ->
  m Wallet.WalletPostingResp
walletPosting = runWithServiceConfig Juspay.walletPosting

walletReversal ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Wallet.WalletReversalReq ->
  m Wallet.WalletReversalResp
walletReversal = runWithServiceConfig Juspay.walletReversal

walletBalance ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Wallet.WalletBalanceReq ->
  m Wallet.WalletBalanceResp
walletBalance = runWithServiceConfig Juspay.walletBalance

walletVerifyTxn ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Wallet.WalletVerifyTxnReq ->
  m Wallet.WalletVerifyTxnResp
walletVerifyTxn = runWithServiceConfig Juspay.walletVerifyTxn

runWithServiceConfig ::
  ( EncFlow m r,
    CoreMetrics m,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  (Juspay.JuspayWalletConfig -> req -> m resp) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func merchantId merchantOperatingCityId req = do
  let serviceName = DMSC.WalletService Wallet.Juspay
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity serviceName merchantOperatingCityId
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantId.getId "Wallet" (show Wallet.Juspay))
  case merchantServiceConfig.serviceConfig of
    DMSC.WalletServiceConfig walletCfg ->
      case walletCfg of
        Wallet.JuspayWalletConfig juspayCfg -> func juspayCfg req
    _ -> throwError $ InternalError "Unknown Service Config"
