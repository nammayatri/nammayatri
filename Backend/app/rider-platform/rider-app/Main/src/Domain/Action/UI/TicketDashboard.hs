module Domain.Action.UI.TicketDashboard where

import qualified API.Types.Dashboard.AppManagement.Tickets as Tickets
import qualified AWS.S3 as S3
import qualified Data.Text as T
import qualified Domain.Types.MerchantOnboarding as MO
import Environment
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.TicketMerchantDetails as QTMD
import Tools.Error

getTicketDashboardUserInfo :: Text -> MO.RequestorRole -> Environment.Flow Tickets.TicketDashboardUserInfo
getTicketDashboardUserInfo userId userRole = do
  person <- QP.findById (Id userId) >>= fromMaybeM (PersonNotFound userId)
  mbTicketMerchantDetails <- QTMD.findById (Id userId)
  mobileNumber <- case person.mobileNumber of
    Just encMobNum -> decrypt encMobNum
    Nothing -> throwError $ InternalError "Mobile number not found"

  let mbDecryptedFields = case mbTicketMerchantDetails of
        Nothing -> pure (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
        Just details -> do
          mbAgreementLetter <- mapM decrypt details.agreementLetter
          bankAccountNumber <- decrypt details.bankAccountNumber
          bankIfsc <- decrypt details.bankIfsc
          mbDocCancelledCheque <- mapM decrypt details.docCancelledCheque
          docPan <- decrypt details.docPan
          mbGstin <- mapM decrypt details.gstin
          pan <- decrypt details.pan
          pure (mbAgreementLetter, Just bankAccountNumber, Just bankIfsc, mbDocCancelledCheque, Just docPan, mbGstin, Just pan)

  (mbDecryptedAgreementLetter, mbDecryptedBankAccountNumber, mbDecryptedBankIfsc, mbDecryptedDocCancelledCheque, mbDecryptedDocPan, mbDecryptedGstin, mbDecryptedPan) <- mbDecryptedFields

  return
    Tickets.TicketDashboardUserInfo
      { firstName = person.firstName,
        lastName = person.lastName,
        role = userRole,
        registeredNumber = mobileNumber,
        agreementLetter = mbDecryptedAgreementLetter,
        bankAccountNumber = mbDecryptedBankAccountNumber,
        bankAccountType = mbTicketMerchantDetails >>= \details -> Just details.bankAccountType,
        bankBeneficiaryName = mbTicketMerchantDetails >>= \details -> Just details.bankBeneficiaryName,
        bankIfsc = mbDecryptedBankIfsc,
        contactDetails = mbTicketMerchantDetails >>= \details -> Just details.contactDetails,
        docCancelledCheque = mbDecryptedDocCancelledCheque,
        docPan = mbDecryptedDocPan,
        gstin = mbDecryptedGstin,
        orgAddress = mbTicketMerchantDetails >>= (.orgAddress),
        orgName = mbTicketMerchantDetails >>= \details -> Just details.orgName,
        pan = mbDecryptedPan,
        state = mbTicketMerchantDetails >>= \details -> Just details.state
      }

getTicketDashboardFile :: Text -> Environment.Flow MO.GetFileResponse
getTicketDashboardFile fileId = do
  file <- MFQuery.findById (Id fileId) >>= fromMaybeM (InvalidRequest "No file found")
  filePath <- file.s3FilePath & fromMaybeM (FileDoNotExist fileId)
  base64File <- S3.get $ T.unpack filePath
  return $
    MO.GetFileResponse
      { fileBase64 = base64File,
        fileType = show file._type
      }
