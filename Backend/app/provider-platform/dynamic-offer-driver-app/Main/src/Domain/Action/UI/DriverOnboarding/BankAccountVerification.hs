{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.BankAccountVerification
  ( DriverBankAccountVerifyReq (..),
    verifyBankAccount,
    getInfoBankAccount,
    deleteBankAccount
  )
where

import qualified Domain.Types.DriverBankAccount as DDBA
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Verification as KEV
import Kernel.External.Verification.Interface.Types
import qualified Kernel.External.Verification.Types as VerificationTypes
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, throwError)
import Kernel.Utils.Logging (logDebug)
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Person as Person
import qualified Tools.Verification as Verification
import Kernel.Types.APISuccess

data DriverBankAccountVerifyReq = DriverBankAccountVerifyReq
  { bankAccountNo :: Text,
    bankIfscCode :: Text,
    nfVerification :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

verifyBankAccount ::
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverBankAccountVerifyReq ->
  Flow Kernel.External.Verification.Interface.Types.VerifyAsyncResp
verifyBankAccount (personId, _merchantId, merchantOpCityId) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mbExistingAccount <- runInReplica $ QDBA.findByPrimaryKey person.id
  when (isJust mbExistingAccount) $
    throwError $ InvalidRequest "Bank account already present"
  verifyRes <-
    Verification.verifyBankAccountAsync person.merchantId merchantOpCityId $
      Verification.VerifyBankAccountAsyncReq
        { bankAccountNo = req.bankAccountNo,
          bankIfscCode = req.bankIfscCode,
          nfVerification = req.nfVerification,
          driverId = person.id.getId
        }
  return verifyRes

-- | Placeholder image Id for bank account verification (no document image); used only to satisfy IdfyVerification schema.
getInfoBankAccount :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> Flow VerificationTypes.BankAccountVerificationResponse
getInfoBankAccount (personId, merchantId, merchantOpCityId) requestId = do
  rsp <- Verification.getTask merchantOpCityId KEV.Idfy (KEV.GetTaskReq Nothing requestId) IVQuery.updateResponse
  case rsp of
    KEV.BankAccountResp resp -> do
      logDebug $ "Bank account verification response: " <> show resp
      now <- getCurrentTime
      let driverBankAccount =
            DDBA.DriverBankAccount
              { accountId = fromMaybe "" resp.bankAccountNumber,
                chargesEnabled = False,
                currentAccountLink = Nothing,
                currentAccountLinkExpiry = Nothing,
                detailsSubmitted = False,
                driverId = personId,
                merchantId = Just merchantId,
                merchantOperatingCityId = Just merchantOpCityId,
                paymentMode = Nothing,
                createdAt = now,
                updatedAt = now,
                ifscCode = resp.ifscCode,
                nameAtBank = resp.nameAtBank
              }
      when (resp.accountExists) $
        QDBA.create driverBankAccount
      let driverBankAccountDetails =
            DDI.DriverBankAccountDetails
              { accountNumber = resp.bankAccountNumber,
                ifscCode = driverBankAccount.ifscCode,
                nameAtBank = driverBankAccount.nameAtBank
              }
      QDI.updateDriverBankAccountDetails (Just driverBankAccountDetails) personId
      return resp
    _ -> throwError $ InternalError "verification response and apiEndpoint mismatch occurred !!!!!!!!"

deleteBankAccount :: (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Flow APISuccess
deleteBankAccount (personId, _merchantId, _merchantOpCityId) = do
  QDBA.deleteById personId
  QDI.updateDriverBankAccountDetails Nothing personId
  pure Success
