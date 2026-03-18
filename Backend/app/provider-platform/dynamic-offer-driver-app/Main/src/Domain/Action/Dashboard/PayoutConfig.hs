{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.PayoutConfig
  ( getFeeConfig,
    updateFeeConfig,
    getLimits,
    updateLimits,
    getDriverPayoutMethods,
    blockPayoutMethod,
    UpdateFeeConfigReq (..),
    UpdateLimitsReq (..),
    BlockPayoutMethodReq (..),
    PayoutMethodResp (..),
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutFeeConfig as DPFC
import qualified Domain.Types.PayoutLimit as DPL
import qualified Domain.Types.PayoutMethod as DPM
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.PayoutFeeConfig as QPFC
import qualified Storage.Queries.PayoutLimit as QPL
import qualified Storage.Queries.PayoutMethod as QPM

-- | Response for a driver's payout method (admin view).
data PayoutMethodResp = PayoutMethodResp
  { id :: Id DPM.PayoutMethod,
    driverId :: Id DP.Person,
    methodType :: DPM.PayoutMethodType,
    vpa :: Maybe Text,
    bankAccountNumber :: Maybe Text,
    ifscCode :: Maybe Text,
    bankName :: Maybe Text,
    beneficiaryName :: Maybe Text,
    isPrimary :: Bool,
    verificationStatus :: DPM.VerificationStatus,
    isBlocked :: Bool,
    blockedReason :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Request to update fee config.
data UpdateFeeConfigReq = UpdateFeeConfigReq
  { methodType :: DPM.PayoutMethodType,
    payoutType :: DPFC.PayoutType,
    feePercentage :: Maybe HighPrecMoney,
    fixedFee :: Maybe HighPrecMoney,
    minFee :: Maybe HighPrecMoney,
    maxFee :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Request to update payout limits.
data UpdateLimitsReq = UpdateLimitsReq
  { dailyLimit :: Maybe HighPrecMoney,
    weeklyLimit :: Maybe HighPrecMoney,
    monthlyLimit :: Maybe HighPrecMoney,
    perTransactionMin :: Maybe HighPrecMoney,
    perTransactionMax :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Request to block a payout method.
data BlockPayoutMethodReq = BlockPayoutMethodReq
  { reason :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- getFeeConfig
--------------------------------------------------------------------------------

getFeeConfig ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow [DPFC.PayoutFeeConfig]
getFeeConfig merchantShortId opCity = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  QPFC.findByMerchantOpCityId merchantOpCity.id

--------------------------------------------------------------------------------
-- updateFeeConfig
--------------------------------------------------------------------------------

updateFeeConfig ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  UpdateFeeConfigReq ->
  Environment.Flow APISuccess
updateFeeConfig merchantShortId opCity req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  mbExisting <- QPFC.findByMerchantOpCityIdAndMethodTypeAndPayoutType merchantOpCity.id req.methodType req.payoutType
  case mbExisting of
    Just existing -> do
      now <- getCurrentTime
      let updated =
            existing
              { DPFC.feePercentage = fromMaybe existing.feePercentage req.feePercentage,
                DPFC.fixedFee = fromMaybe existing.fixedFee req.fixedFee,
                DPFC.minFee = fromMaybe existing.minFee req.minFee,
                DPFC.maxFee = fromMaybe existing.maxFee req.maxFee,
                DPFC.updatedAt = now
              }
      QPFC.updateByPrimaryKey updated
      logInfo $ "Updated PayoutFeeConfig for " <> show req.methodType <> "/" <> show req.payoutType <> " in city " <> merchantOpCity.id.getId
    Nothing -> do
      now <- getCurrentTime
      newId <- generateGUID
      let newConfig =
            DPFC.PayoutFeeConfig
              { DPFC.id = newId,
                DPFC.merchantId = merchant.id,
                DPFC.merchantOperatingCityId = merchantOpCity.id,
                DPFC.methodType = req.methodType,
                DPFC.payoutType = req.payoutType,
                DPFC.feePercentage = fromMaybe 0 req.feePercentage,
                DPFC.fixedFee = fromMaybe 0 req.fixedFee,
                DPFC.minFee = fromMaybe 0 req.minFee,
                DPFC.maxFee = fromMaybe 0 req.maxFee,
                DPFC.createdAt = now,
                DPFC.updatedAt = now
              }
      QPFC.create newConfig
      logInfo $ "Created PayoutFeeConfig for " <> show req.methodType <> "/" <> show req.payoutType <> " in city " <> merchantOpCity.id.getId
  pure Success

--------------------------------------------------------------------------------
-- getLimits
--------------------------------------------------------------------------------

getLimits ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Environment.Flow (Maybe DPL.PayoutLimit)
getLimits merchantShortId opCity = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  QPL.findByMerchantOpCityId merchantOpCity.id

--------------------------------------------------------------------------------
-- updateLimits
--------------------------------------------------------------------------------

updateLimits ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  UpdateLimitsReq ->
  Environment.Flow APISuccess
updateLimits merchantShortId opCity req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  mbExisting <- QPL.findByMerchantOpCityId merchantOpCity.id
  case mbExisting of
    Just existing -> do
      now <- getCurrentTime
      let updated =
            existing
              { DPL.dailyLimit = fromMaybe existing.dailyLimit req.dailyLimit,
                DPL.weeklyLimit = fromMaybe existing.weeklyLimit req.weeklyLimit,
                DPL.monthlyLimit = fromMaybe existing.monthlyLimit req.monthlyLimit,
                DPL.perTransactionMin = fromMaybe existing.perTransactionMin req.perTransactionMin,
                DPL.perTransactionMax = fromMaybe existing.perTransactionMax req.perTransactionMax,
                DPL.updatedAt = now
              }
      QPL.updateByPrimaryKey updated
      logInfo $ "Updated PayoutLimit for city " <> merchantOpCity.id.getId
    Nothing -> do
      now <- getCurrentTime
      newId <- generateGUID
      let newLimit =
            DPL.PayoutLimit
              { DPL.id = newId,
                DPL.merchantId = merchant.id,
                DPL.merchantOperatingCityId = merchantOpCity.id,
                DPL.dailyLimit = fromMaybe 0 req.dailyLimit,
                DPL.weeklyLimit = fromMaybe 0 req.weeklyLimit,
                DPL.monthlyLimit = fromMaybe 0 req.monthlyLimit,
                DPL.perTransactionMin = fromMaybe 0 req.perTransactionMin,
                DPL.perTransactionMax = fromMaybe 0 req.perTransactionMax,
                DPL.createdAt = now,
                DPL.updatedAt = now
              }
      QPL.create newLimit
      logInfo $ "Created PayoutLimit for city " <> merchantOpCity.id.getId
  pure Success

--------------------------------------------------------------------------------
-- getDriverPayoutMethods
--------------------------------------------------------------------------------

getDriverPayoutMethods ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id DP.Person ->
  Environment.Flow [PayoutMethodResp]
getDriverPayoutMethods _merchantShortId _opCity driverId = do
  methods <- QPM.findByDriverId driverId
  pure $ map toPayoutMethodResp methods

toPayoutMethodResp :: DPM.PayoutMethod -> PayoutMethodResp
toPayoutMethodResp pm =
  PayoutMethodResp
    { id = pm.id,
      driverId = pm.driverId,
      methodType = pm.methodType,
      vpa = pm.vpa,
      bankAccountNumber = pm.bankAccountNumber,
      ifscCode = pm.ifscCode,
      bankName = pm.bankName,
      beneficiaryName = pm.beneficiaryName,
      isPrimary = pm.isPrimary,
      verificationStatus = pm.verificationStatus,
      isBlocked = pm.isBlocked,
      blockedReason = pm.blockedReason,
      createdAt = pm.createdAt,
      updatedAt = pm.updatedAt
    }

--------------------------------------------------------------------------------
-- blockPayoutMethod
--------------------------------------------------------------------------------

blockPayoutMethod ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Id DP.Person ->
  Id DPM.PayoutMethod ->
  BlockPayoutMethodReq ->
  Environment.Flow APISuccess
blockPayoutMethod _merchantShortId _opCity driverId methodId req = do
  method <- QPM.findById methodId >>= fromMaybeM (InvalidRequest "Payout method not found")
  unless (method.driverId == driverId) $
    throwError $ InvalidRequest "Payout method does not belong to this driver"
  now <- getCurrentTime
  QPM.updateBlockStatus True (Just req.reason) now methodId
  -- If this was the primary method, unset it
  when method.isPrimary $
    QPM.updateIsPrimary False now methodId
  logInfo $ "Blocked payout method " <> methodId.getId <> " for driver " <> driverId.getId <> " reason: " <> req.reason
  pure Success
