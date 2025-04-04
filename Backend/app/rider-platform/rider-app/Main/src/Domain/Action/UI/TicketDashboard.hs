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
  return
    Tickets.TicketDashboardUserInfo
      { firstName = person.firstName,
        lastName = person.lastName,
        role = userRole,
        registeredNumber = mobileNumber,
        agreementLetter = mbTicketMerchantDetails >>= (.agreementLetter) >>= \encAgreement -> Just $ unEncrypted encAgreement.encrypted,
        bankAccountNumber = mbTicketMerchantDetails >>= \details -> Just $ unEncrypted details.bankAccountNumber.encrypted,
        bankAccountType = mbTicketMerchantDetails >>= \details -> Just details.bankAccountType,
        bankBeneficiaryName = mbTicketMerchantDetails >>= \details -> Just details.bankBeneficiaryName,
        bankIfsc = mbTicketMerchantDetails >>= \details -> Just $ unEncrypted details.bankIfsc.encrypted,
        contactDetails = mbTicketMerchantDetails >>= \details -> Just details.contactDetails,
        docCancelledCheque = mbTicketMerchantDetails >>= (.docCancelledCheque) >>= \encDoc -> Just $ unEncrypted encDoc.encrypted,
        docPan = mbTicketMerchantDetails >>= \details -> Just $ unEncrypted details.docPan.encrypted,
        gstin = mbTicketMerchantDetails >>= (.gstin) >>= \encGstin -> Just $ unEncrypted encGstin.encrypted,
        orgAddress = mbTicketMerchantDetails >>= (.orgAddress),
        orgName = mbTicketMerchantDetails >>= \details -> Just details.orgName,
        pan = mbTicketMerchantDetails >>= \details -> Just $ unEncrypted details.pan.encrypted,
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
