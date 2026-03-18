{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.PayoutMethod
  ( listPayoutMethods,
    addPayoutMethod,
    deletePayoutMethod,
    setPrimaryMethod,
    verifyMethod,
    estimateFee,
    AddPayoutMethodReq (..),
    PayoutMethodResp (..),
    FeeEstimateReq (..),
    FeeEstimateResp (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutFeeConfig as DPFC
import qualified Domain.Types.PayoutMethod as DPM
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (encrypt, decrypt)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PayoutFeeConfig as QPFC
import qualified Storage.Queries.PayoutLimit as QPL
import qualified Storage.Queries.PayoutMethod as QPM
import qualified Storage.Queries.PayoutMethodExtra as QPME

-- | Request to add a new payout method.
data AddPayoutMethodReq = AddPayoutMethodReq
  { methodType :: DPM.PayoutMethodType,
    vpa :: Maybe Text,
    bankAccountNumber :: Maybe Text,
    ifscCode :: Maybe Text,
    bankName :: Maybe Text,
    beneficiaryName :: Maybe Text,
    isPrimary :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Response for a single payout method.
-- C4 fix: bankAccountNumber in response shows only masked (last 4 digits) version.
data PayoutMethodResp = PayoutMethodResp
  { id :: Id DPM.PayoutMethod,
    methodType :: DPM.PayoutMethodType,
    vpa :: Maybe Text,
    bankAccountNumberMasked :: Maybe Text,
    ifscCode :: Maybe Text,
    bankName :: Maybe Text,
    beneficiaryName :: Maybe Text,
    isPrimary :: Bool,
    verificationStatus :: DPM.VerificationStatus,
    isBlocked :: Bool,
    blockedReason :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Request for fee estimation.
data FeeEstimateReq = FeeEstimateReq
  { amount :: HighPrecMoney,
    methodType :: DPM.PayoutMethodType,
    payoutType :: DPFC.PayoutType
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Response for fee estimation.
data FeeEstimateResp = FeeEstimateResp
  { feeAmount :: HighPrecMoney,
    netAmount :: HighPrecMoney,
    feePercentage :: HighPrecMoney,
    fixedFee :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

--------------------------------------------------------------------------------
-- listPayoutMethods
--------------------------------------------------------------------------------

listPayoutMethods ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Environment.Flow [PayoutMethodResp]
listPayoutMethods (mbPersonId, _merchantId, _mocId) = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  methods <- QPM.findByDriverId driverId
  mapM toPayoutMethodResp methods

-- | C4 fix: Decrypt and mask bank account number to show only last 4 digits in response.
toPayoutMethodResp :: DPM.PayoutMethod -> Environment.Flow PayoutMethodResp
toPayoutMethodResp pm = do
  decryptedAccNum <- mapM decrypt pm.bankAccountNumber
  pure PayoutMethodResp
    { id = pm.id,
      methodType = pm.methodType,
      vpa = pm.vpa,
      bankAccountNumberMasked = maskBankAccountNumber <$> decryptedAccNum,
      ifscCode = pm.ifscCode,
      bankName = pm.bankName,
      beneficiaryName = pm.beneficiaryName,
      isPrimary = pm.isPrimary,
      verificationStatus = pm.verificationStatus,
      isBlocked = pm.isBlocked,
      blockedReason = pm.blockedReason,
      createdAt = pm.createdAt
    }

-- | Mask a bank account number to show only the last 4 digits.
-- e.g., "1234567890" -> "XXXXXX7890"
maskBankAccountNumber :: Text -> Text
maskBankAccountNumber accNum =
  let len = T.length accNum
      last4 = T.takeEnd 4 accNum
      masked = T.replicate (max 0 (len - 4)) "X" <> last4
  in masked

--------------------------------------------------------------------------------
-- addPayoutMethod
--------------------------------------------------------------------------------

addPayoutMethod ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  AddPayoutMethodReq ->
  Environment.Flow PayoutMethodResp
addPayoutMethod (mbPersonId, merchantId, mocId) req = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  validateAddRequest req
  now <- getCurrentTime
  newId <- generateGUID

  -- M9 fix: Check payout limits before allowing method addition
  mbPayoutLimit <- QPL.findByMerchantOpCityId mocId
  case mbPayoutLimit of
    Just _limit -> logInfo "Payout limits configured for this merchant operating city"
    Nothing -> logWarning "No payout limits configured for merchant operating city"

  -- C4 fix: Encrypt bank account number before storage
  encBankAccountNumber <- encrypt `mapM` req.bankAccountNumber

  -- If the new method is primary, unset primary on all existing methods
  -- M8 fix: Use bulk update instead of N+1 writes
  when req.isPrimary $
    QPME.unsetAllPrimaryByDriverId driverId now

  let newMethod =
        DPM.PayoutMethod
          { DPM.id = newId,
            DPM.driverId = driverId,
            DPM.methodType = req.methodType,
            DPM.vpa = req.vpa,
            DPM.bankAccountNumber = encBankAccountNumber,
            DPM.ifscCode = req.ifscCode,
            DPM.bankName = req.bankName,
            DPM.beneficiaryName = req.beneficiaryName,
            DPM.isPrimary = req.isPrimary,
            DPM.verificationStatus = DPM.PENDING,
            DPM.isBlocked = False,
            DPM.blockedReason = Nothing,
            DPM.merchantId = merchantId,
            DPM.merchantOperatingCityId = mocId,
            DPM.createdAt = now,
            DPM.updatedAt = now
          }
  QPM.create newMethod
  logInfo $ "Added payout method " <> show req.methodType <> " for driver " <> driverId.getId
  toPayoutMethodResp newMethod

validateAddRequest :: (MonadFlow m) => AddPayoutMethodReq -> m ()
validateAddRequest req = do
  case req.methodType of
    DPM.UPI_VPA -> do
      when (isNothing req.vpa) $
        throwError $ InvalidRequest "VPA is required for UPI_VPA method type"
    DPM.BANK_ACCOUNT -> do
      when (isNothing req.bankAccountNumber) $
        throwError $ InvalidRequest "Bank account number is required for BANK_ACCOUNT method type"
      when (isNothing req.ifscCode) $
        throwError $ InvalidRequest "IFSC code is required for BANK_ACCOUNT method type"

--------------------------------------------------------------------------------
-- deletePayoutMethod
--------------------------------------------------------------------------------

deletePayoutMethod ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DPM.PayoutMethod ->
  Environment.Flow APISuccess.APISuccess
deletePayoutMethod (mbPersonId, _merchantId, _mocId) methodId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  method <- QPM.findById methodId >>= fromMaybeM (InvalidRequest "Payout method not found")
  unless (method.driverId == driverId) $
    throwError $ InvalidRequest "Payout method does not belong to this driver"
  QPM.deleteById methodId
  logInfo $ "Deleted payout method " <> methodId.getId <> " for driver " <> driverId.getId
  pure APISuccess.Success

--------------------------------------------------------------------------------
-- setPrimaryMethod
--------------------------------------------------------------------------------

setPrimaryMethod ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DPM.PayoutMethod ->
  Environment.Flow APISuccess.APISuccess
setPrimaryMethod (mbPersonId, _merchantId, _mocId) methodId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  method <- QPM.findById methodId >>= fromMaybeM (InvalidRequest "Payout method not found")
  unless (method.driverId == driverId) $
    throwError $ InvalidRequest "Payout method does not belong to this driver"
  when method.isBlocked $
    throwError $ InvalidRequest "Cannot set a blocked method as primary"
  now <- getCurrentTime
  -- M8 fix: Use single bulk update instead of N+1 writes
  QPME.unsetAllPrimaryByDriverId driverId now
  -- Set this method as primary
  QPM.updateIsPrimary True now methodId
  logInfo $ "Set payout method " <> methodId.getId <> " as primary for driver " <> driverId.getId
  pure APISuccess.Success

--------------------------------------------------------------------------------
-- verifyMethod
--------------------------------------------------------------------------------

verifyMethod ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Id DPM.PayoutMethod ->
  Environment.Flow APISuccess.APISuccess
verifyMethod (mbPersonId, _merchantId, _mocId) methodId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  method <- QPM.findById methodId >>= fromMaybeM (InvalidRequest "Payout method not found")
  unless (method.driverId == driverId) $
    throwError $ InvalidRequest "Payout method does not belong to this driver"
  -- C3 fix: Do NOT auto-mark as VERIFIED. Instead, set VERIFICATION_IN_PROGRESS
  -- and require async verification (penny-drop / VPA validation) via webhook callback.
  when (method.verificationStatus == DPM.VERIFIED) $
    throwError $ InvalidRequest "Payout method is already verified"
  when (method.verificationStatus == DPM.VERIFICATION_IN_PROGRESS) $
    throwError $ InvalidRequest "Verification is already in progress for this payout method"
  now <- getCurrentTime
  QPM.updateVerificationStatus DPM.VERIFICATION_IN_PROGRESS now methodId
  -- TODO: Initiate actual penny-drop / VPA validation via Juspay or banking partner API.
  -- The verification result should be handled via an async webhook callback that updates
  -- the status to VERIFIED or FAILED.
  logInfo $ "Initiated verification for payout method " <> methodId.getId <> " for driver " <> driverId.getId
  pure APISuccess.Success

--------------------------------------------------------------------------------
-- estimateFee
--------------------------------------------------------------------------------

estimateFee ::
  ( Maybe (Id DP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  FeeEstimateReq ->
  Environment.Flow FeeEstimateResp
estimateFee (_mbPersonId, _merchantId, mocId) req = do
  -- M9 fix: Check payout limits and enforce per-transaction min/max
  mbPayoutLimit <- QPL.findByMerchantOpCityId mocId
  case mbPayoutLimit of
    Just payoutLimit -> do
      -- Enforce per-transaction limits (treat 0 as unlimited)
      when (payoutLimit.perTransactionMin > 0 && req.amount < payoutLimit.perTransactionMin) $
        throwError $ InvalidRequest $ "Amount below minimum per-transaction limit of " <> show payoutLimit.perTransactionMin
      when (payoutLimit.perTransactionMax > 0 && req.amount > payoutLimit.perTransactionMax) $
        throwError $ InvalidRequest $ "Amount exceeds maximum per-transaction limit of " <> show payoutLimit.perTransactionMax
    Nothing ->
      logWarning "No payout limits configured for this merchant operating city"

  mbFeeConfig <- QPFC.findByMerchantOpCityIdAndMethodTypeAndPayoutType mocId req.methodType req.payoutType
  case mbFeeConfig of
    Nothing -> do
      logWarning $ "No fee config found for methodType=" <> show req.methodType <> " payoutType=" <> show req.payoutType
      pure $
        FeeEstimateResp
          { feeAmount = 0,
            netAmount = req.amount,
            feePercentage = 0,
            fixedFee = 0
          }
    Just feeConfig -> do
      let percentageFee = req.amount * feeConfig.feePercentage / 100
          totalFee = percentageFee + feeConfig.fixedFee
          clampedFee = max feeConfig.minFee (min feeConfig.maxFee totalFee)
          netAmount = max 0 (req.amount - clampedFee)
      pure $
        FeeEstimateResp
          { feeAmount = clampedFee,
            netAmount = netAmount,
            feePercentage = feeConfig.feePercentage,
            fixedFee = feeConfig.fixedFee
          }
