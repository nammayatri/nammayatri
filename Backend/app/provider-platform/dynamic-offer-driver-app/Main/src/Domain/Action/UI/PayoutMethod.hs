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

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PayoutFeeConfig as DPFC
import qualified Domain.Types.PayoutMethod as DPM
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PayoutFeeConfig as QPFC
import qualified Storage.Queries.PayoutMethod as QPM

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
data PayoutMethodResp = PayoutMethodResp
  { id :: Id DPM.PayoutMethod,
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
  pure $ map toPayoutMethodResp methods

toPayoutMethodResp :: DPM.PayoutMethod -> PayoutMethodResp
toPayoutMethodResp pm =
  PayoutMethodResp
    { id = pm.id,
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
      createdAt = pm.createdAt
    }

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
  -- If the new method is primary, unset primary on all existing methods
  when req.isPrimary $ do
    existingMethods <- QPM.findByDriverId driverId
    mapM_ (\m -> QPM.updateIsPrimary False now m.id) existingMethods
  let newMethod =
        DPM.PayoutMethod
          { DPM.id = newId,
            DPM.driverId = driverId,
            DPM.methodType = req.methodType,
            DPM.vpa = req.vpa,
            DPM.bankAccountNumber = req.bankAccountNumber,
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
  pure $ toPayoutMethodResp newMethod

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
  -- Unset primary on all existing methods
  existingMethods <- QPM.findByDriverId driverId
  mapM_ (\m -> QPM.updateIsPrimary False now m.id) existingMethods
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
  -- TODO: Integrate with actual VPA validation / penny-drop service
  -- For now, mark as VERIFIED. In production, this would trigger an async verification
  -- flow via Juspay or banking partner APIs and update status via webhook.
  now <- getCurrentTime
  QPM.updateVerificationStatus DPM.VERIFIED now methodId
  logInfo $ "Verified payout method " <> methodId.getId <> " for driver " <> driverId.getId
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
  mbFeeConfig <- QPFC.findByMerchantOpCityIdAndMethodTypeAndPayoutType mocId req.methodType req.payoutType
  case mbFeeConfig of
    Nothing ->
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
