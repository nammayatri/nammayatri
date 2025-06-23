{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.AppManagement.MerchantOnboarding.TicketHandlers where

import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Types.MerchantOnboarding as MO
import qualified Domain.Types.MerchantOnboarding.Handler as H
import qualified Domain.Types.MerchantOnboardingStep as MOS
import qualified Domain.Types.TicketMerchantDetails as DTMD
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Predicate as P
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.TH
import Kernel.Utils.Validation
import qualified Storage.Queries.MerchantOnboarding as QMO
import qualified Storage.Queries.MerchantOnboardingStep as QMOS
import qualified Storage.Queries.TicketMerchantDetails as QTMD
import Tools.Error

data TicketMerchantInfoPayload = TicketMerchantInfoPayload
  { agreementLetter :: Maybe Text,
    bankAccountNumber :: Text,
    bankAccountType :: DTMD.BankAccountType,
    bankBeneficiaryName :: Text,
    bankIfsc :: Text,
    contactDetails :: DTMD.ContactDetails,
    docCancelledCheque :: Maybe Text,
    docPan :: Text,
    gstin :: Maybe Text,
    orgAddress :: Maybe Text,
    orgName :: Text,
    pan :: Text,
    state :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

handleMerchantDetailsSubmit :: Id MOS.MerchantOnboardingStep -> Value -> Flow H.StepHandlerResult
handleMerchantDetailsSubmit stepId payload = do
  merchantPayload <- fromjson payload
  runRequestValidation validateMerchantDetails merchantPayload
  QMOS.updateStepPayload (Just payload) stepId
  pure
    H.StepHandlerResult
      { success = True,
        message = Just "Merchant details validated successfully",
        nextSteps = [],
        dashboardSideHandler = Nothing
      }

handleMerchantDetailsApprove :: Id MOS.MerchantOnboardingStep -> Value -> Flow H.StepHandlerResult
handleMerchantDetailsApprove stepId _ = do
  step <- QMOS.findByStepId stepId >>= fromMaybeM (InvalidRequest "Step not found")
  onboarding <- QMO.findById (Id step.merchantOnboardingId) >>= fromMaybeM (InvalidRequest "Onboarding Id not found")
  stepPayload <- step.payload & fromMaybeM (InternalError "Step Information not found")
  mp :: TicketMerchantInfoPayload <- fromjson stepPayload
  agreementLetter <- mapM encrypt mp.agreementLetter
  bankAccountNumber <- encrypt mp.bankAccountNumber
  bankIfsc <- encrypt mp.bankIfsc
  docCancelledCheque <- mapM encrypt mp.docCancelledCheque
  docPan <- encrypt mp.docPan
  gstin <- mapM encrypt mp.gstin
  pan <- encrypt mp.pan
  now <- getCurrentTime
  let rid = onboarding.requestorId
  QTMD.create $
    DTMD.TicketMerchantDetails
      { id = Id rid,
        bankAccountType = mp.bankAccountType,
        bankBeneficiaryName = mp.bankBeneficiaryName,
        contactDetails = mp.contactDetails,
        orgAddress = mp.orgAddress,
        orgName = mp.orgName,
        state = mp.state,
        createdAt = now,
        updatedAt = now,
        isBankOnboarded = Just False,
        ..
      }
  pure $
    H.StepHandlerResult
      { success = True,
        message = Just "Merchant Approved successfully",
        nextSteps = [],
        dashboardSideHandler = Nothing
      }

handleBankOnboardingApproval :: Id MOS.MerchantOnboardingStep -> Value -> Flow H.StepHandlerResult
handleBankOnboardingApproval stepId _ = do
  step <- QMOS.findByStepId stepId >>= fromMaybeM (InvalidRequest "Step not found")
  onboarding <- QMO.findById (Id step.merchantOnboardingId) >>= fromMaybeM (InvalidRequest "Onboarding Id not found")
  let rid = onboarding.requestorId
  QTMD.updateIsBankOnboarded (Just True) (Id rid)
  pure $
    H.StepHandlerResult
      { success = True,
        message = Just "Merchant onboarded successfully",
        nextSteps = [],
        dashboardSideHandler = Just $ H.DashboardSideHandler H.SET_ROLE_TICKET_DASHBOARD_MERCHANT [("rid", rid)]
      }

validateMerchantDetails :: Validate TicketMerchantInfoPayload
validateMerchantDetails TicketMerchantInfoPayload {..} =
  sequenceA_
    [ validateField "orgName" orgName $ P.MinLength 3,
      validateField "orgAddress" orgAddress $ P.InMaybe P.NotEmpty,
      validateField "state" state P.NotEmpty,
      validateField "pan" pan $ P.NotEmpty,
      validateField "gstin" gstin $ P.InMaybe $ P.NotEmpty,
      validateField "bankAccountNumber" bankAccountNumber $ P.MinLength 10,
      validateField "bankBeneficiaryName" bankBeneficiaryName $ P.MinLength 3,
      validateObject "contactDetails" contactDetails validateContactDetails
    ]

validateContactDetails :: Validate DTMD.ContactDetails
validateContactDetails DTMD.ContactDetails {..} =
  sequenceA_
    [ validateField "email" email $ P.NotEmpty,
      validateField "name" name $ P.MinLength 3,
      validateField "number" number $ P.mobileNumber
    ]

fromjson :: (FromJSON a) => Data.Aeson.Value -> Environment.Flow a
fromjson val = case Data.Aeson.fromJSON val of
  Data.Aeson.Success x -> return x
  Data.Aeson.Error err -> throwError (InvalidRequest $ "JSON parsing error: " <> T.pack err)
