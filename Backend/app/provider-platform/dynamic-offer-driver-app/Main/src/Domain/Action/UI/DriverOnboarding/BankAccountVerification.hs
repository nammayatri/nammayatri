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
    deleteBankAccount,
  )
where

import qualified Domain.Types.DocumentAuditLog as DAL
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
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, getCurrentTime, throwError)
import Kernel.Utils.Logging (logDebug)
import qualified SharedLogic.DriverOnboarding.Audit as Audit
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Person as Person
import qualified Tools.Verification as Verification

data DriverBankAccountVerifyReq = DriverBankAccountVerifyReq
  { bankAccountNo :: Text,
    bankIfscCode :: Text,
    nfVerification :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

verifyBankAccount ::
  -- | Audit actor. 'Nothing' ⇒ the driver verifying their own bank account (resolved from the person's role);
  -- 'Just' ⇒ a dashboard operator verifying on the driver's behalf.
  Maybe Audit.Requestor ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  DriverBankAccountVerifyReq ->
  Flow Kernel.External.Verification.Interface.Types.VerifyAsyncResp
verifyBankAccount mbRequestor (personId, _merchantId, merchantOpCityId) req = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  verifyRes <-
    Verification.verifyBankAccountAsync person.merchantId merchantOpCityId $
      Verification.VerifyBankAccountAsyncReq
        { bankAccountNo = req.bankAccountNo,
          bankIfscCode = req.bankIfscCode,
          nfVerification = req.nfVerification,
          driverId = person.id.getId
        }
  -- Document audit: bank-account verification requested. eventId = verifyRes.requestId links this
  -- request-time row to the later async getInfoBankAccount result (which carries the same requestId).
  -- Entity from the owner's role: fleet owners verify bank accounts via the payout-account flow too.
  let requestor = Audit.auditActorFromPersonOrRequestor person mbRequestor
  Audit.auditDocStatusWithEvent requestor (Audit.entityTypeFromRole person.role) person.id.getId "BankAccount" DAL.DRIVER_BANK_ACCOUNT (Just person.id.getId) Nothing (Just "PENDING") DAL.VERIFICATION_REQUESTED Nothing (Just verifyRes.requestId) person.merchantId person.merchantOperatingCityId
  return verifyRes

-- | Placeholder image Id for bank account verification (no document image); used only to satisfy IdfyVerification schema.
getInfoBankAccount :: Maybe Audit.Requestor -> (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Text -> Flow VerificationTypes.BankAccountVerificationResponse
getInfoBankAccount mbRequestor (personId, merchantId, merchantOpCityId) requestId = do
  rsp <- Verification.getTask merchantOpCityId KEV.Idfy (KEV.GetTaskReq Nothing requestId) IVQuery.updateResponse
  case rsp of
    KEV.BankAccountResp resp -> do
      logDebug $ "Bank account verification response: " <> show resp
      now <- getCurrentTime
      let driverBankAccount =
            DDBA.DriverBankAccount
              { accountId = fromMaybe "" resp.bankAccountNumber,
                chargesEnabled = False,
                payoutsEnabled = Nothing,
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
      when (resp.accountExists) $ do
        mbExistingAccount <- runInReplica $ QDBA.findByPrimaryKey personId
        case mbExistingAccount of
          Just _ -> QDBA.updateByPrimaryKey driverBankAccount
          Nothing -> QDBA.create driverBankAccount
      -- Document audit: bank-account verification result (VALID when the account exists, else INVALID).
      -- eventId = requestId correlates this async result with the request-time VERIFICATION_REQUESTED row.
      -- Only personId is in scope here: audit-gated owner fetch so a fleet owner's result row
      -- isn't misattributed to the DRIVER entity.
      mbOwner <- Audit.fetchForAuditByCity merchantOpCityId (Person.findById personId)
      Audit.auditDocStatusWithEvent (fromMaybe (maybe (Audit.driverAppPerson personId "DRIVER") (\owner -> Audit.driverAppPerson owner.id (Audit.toActorRole owner.role)) mbOwner) mbRequestor) (Audit.entityTypeFromMbPerson mbOwner) personId.getId "BankAccount" DAL.DRIVER_BANK_ACCOUNT (Just personId.getId) Nothing (Just (if resp.accountExists then "VALID" else "INVALID")) DAL.STATUS_CHANGED Nothing (Just requestId) merchantId merchantOpCityId
      let driverBankAccountDetails =
            DDI.DriverBankAccountDetails
              { accountNumber = resp.bankAccountNumber,
                ifscCode = driverBankAccount.ifscCode,
                nameAtBank = driverBankAccount.nameAtBank
              }
      QDI.updateDriverBankAccountDetails (Just driverBankAccountDetails) personId
      return resp
    _ -> throwError $ InternalError "verification response and apiEndpoint mismatch occurred !!!!!!!!"

deleteBankAccount ::
  -- | Audit actor. 'Nothing' ⇒ the driver deleting their own bank account; 'Just' ⇒ a dashboard operator.
  Maybe Audit.Requestor ->
  (Id Person.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Flow APISuccess
deleteBankAccount mbRequestor (personId, _merchantId, _merchantOpCityId) = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  QDBA.deleteById personId
  QDI.updateDriverBankAccountDetails Nothing personId
  let requestor = Audit.auditActorFromPersonOrRequestor person mbRequestor
  -- Entity from the owner's role: fleet owners also hold (and delete) bank accounts.
  Audit.auditDelete requestor (Audit.entityTypeFromRole person.role) person.id.getId "BankAccount" DAL.DRIVER_BANK_ACCOUNT (Just person.id.getId) Nothing person.merchantId person.merchantOperatingCityId
  pure Success
