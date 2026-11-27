-- | Request and response types shared by the public and dashboard permit APIs.
-- 'PermitDetails' has one optional field per permit type.
module Lib.Permit.Api.Types where

import Data.Time (Day)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.HideSecrets
import Kernel.Types.Id
import Lib.Permit.Domain.Types.PermitApplication
import Lib.Permit.Domain.Types.PermitApplicationDocument (PermitDocumentType)
import Lib.Permit.Domain.Types.PermitApplicationEvent (PermitActorType)
import Lib.Permit.Picnic.Types
import Lib.Permit.Types

newtype PermitDetails = PermitDetails
  { picnic :: Maybe PicnicDetails
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PicnicSummary = PicnicSummary
  { leaderName :: Text,
    vehicleRegistrationNo :: Text,
    spot :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitApplicationListItem = PermitApplicationListItem
  { applicationId :: Id PermitApplication,
    referenceNo :: Text,
    permitType :: PermitType,
    status :: PermitStatus,
    approvalLevel :: ApprovalLevel,
    permitDate :: Day,
    submittedAt :: Maybe UTCTime,
    picnic :: Maybe PicnicSummary
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

newtype PermitApplicationListRes = PermitApplicationListRes
  { applications :: [PermitApplicationListItem]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitDocumentInfo = PermitDocumentInfo
  { documentType :: PermitDocumentType,
    validUntil :: Maybe Day,
    lifetime :: Bool,
    contentType :: Text,
    uploadedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitDecisionInfo = PermitDecisionInfo
  { approved :: Bool,
    reason :: Text,
    decidedByName :: Text,
    decidedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitHistoryItem = PermitHistoryItem
  { action :: Text,
    actorType :: PermitActorType,
    actorName :: Maybe Text,
    reason :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitApplicationDetailsRes = PermitApplicationDetailsRes
  { applicationId :: Id PermitApplication,
    referenceNo :: Text,
    permitType :: PermitType,
    status :: PermitStatus,
    approvalLevel :: ApprovalLevel,
    permitDate :: Day,
    validFrom :: UTCTime,
    validTill :: UTCTime,
    submittedAt :: Maybe UTCTime,
    details :: PermitDetails,
    documents :: [PermitDocumentInfo],
    ruleFailures :: [RuleFailure],
    decision :: Maybe PermitDecisionInfo,
    history :: [PermitHistoryItem]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitDocumentRes = PermitDocumentRes
  { contentType :: Text,
    imageBase64 :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitDecisionReq = PermitDecisionReq
  { approve :: Bool,
    reason :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance HideSecrets PermitDecisionReq where
  hideSecrets = identity

data PermitVerifyPassReq = PermitVerifyPassReq
  { qrText :: Maybe Text,
    referenceNo :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | The QR text lets its holder show a pass, so it is kept out of the dashboard audit log.
instance HideSecrets PermitVerifyPassReq where
  hideSecrets req = PermitVerifyPassReq {qrText = "hidden" <$ req.qrText, referenceNo = req.referenceNo}

data PicnicPassInfo = PicnicPassInfo
  { vehicleRegistrationNo :: Text,
    driverName :: Text,
    driverLicenceNo :: Text,
    leaderName :: Text,
    passengerNames :: [Text],
    handymanName :: Maybe Text,
    travelDate :: Day,
    departureTime :: Text,
    returnTime :: Text,
    returnDeadline :: Text,
    startPoint :: Text,
    spot :: Text,
    destinationDistrict :: Text,
    routeOut :: Text,
    routeBack :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitPassInfo = PermitPassInfo
  { referenceNo :: Text,
    permitType :: PermitType,
    status :: PermitStatus,
    picnic :: Maybe PicnicPassInfo
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data PermitVerifyPassRes = PermitVerifyPassRes
  { result :: ScanResult,
    reasons :: [ScanReason],
    intensifiedCheck :: Bool,
    pass :: Maybe PermitPassInfo
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- Public (citizen) APIs -------------------------------------------------------

data PermitAuthOtpReq = PermitAuthOtpReq
  { merchantShortId :: Text,
    city :: Context.City,
    mobileCountryCode :: Text,
    mobileNumber :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitAuthVerifyReq = PermitAuthVerifyReq
  { merchantShortId :: Text,
    city :: Context.City,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    otp :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PermitAuthVerifyRes = PermitAuthVerifyRes
  { token :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitCreateApplicationReq = PermitCreateApplicationReq
  { permitType :: PermitType,
    details :: PermitDetails
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitCreateApplicationRes = PermitCreateApplicationRes
  { applicationId :: Id PermitApplication,
    referenceNo :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PermitUpdateApplicationReq = PermitUpdateApplicationReq
  { details :: PermitDetails
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitDocumentUploadReq = PermitDocumentUploadReq
  { documentType :: PermitDocumentType,
    imageBase64 :: Text,
    contentType :: Text,
    validUntil :: Maybe Day,
    lifetime :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitMyApplicationRes = PermitMyApplicationRes
  { applicationId :: Id PermitApplication,
    referenceNo :: Text,
    permitType :: PermitType,
    status :: PermitStatus,
    approvalLevel :: ApprovalLevel,
    details :: PermitDetails,
    documents :: [PermitDocumentInfo],
    requiredDocuments :: [PermitDocumentType],
    -- | For a draft: everything that currently stops submission. Otherwise empty.
    ruleFailures :: [RuleFailure],
    decisionReason :: Maybe Text,
    decidedAt :: Maybe UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | One optional field per permit type, like 'PermitDetails'.
newtype PermitLimitsRes = PermitLimitsRes
  { picnic :: Maybe PicnicLimits
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitUndertakingOtpRes = PermitUndertakingOtpRes
  { otpSent :: Bool,
    ruleFailures :: [RuleFailure]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitSubmitReq = PermitSubmitReq
  { applicantOtp :: Text,
    ownerOtp :: Text,
    acceptUndertaking :: Bool,
    ownerCertified :: Bool,
    dataConsent :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PermitPassRes = PermitPassRes
  { qrText :: Text,
    pass :: PermitPassInfo
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
