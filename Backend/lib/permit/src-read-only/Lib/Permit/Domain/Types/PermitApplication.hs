{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Domain.Types.PermitApplication where

import Data.Aeson
import qualified Data.Time
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Permit.Domain.Types.Common
import qualified Lib.Permit.Domain.Types.PermitApplicant
import qualified Lib.Permit.Types
import qualified Tools.Beam.UtilsTH

data PermitApplicationE e = PermitApplication
  { applicantId :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant,
    approvalLevel :: Lib.Permit.Domain.Types.PermitApplication.ApprovalLevel,
    consentAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    decidedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    decidedById :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    decidedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    decisionReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    details :: Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.PermitApplication.PermitApplication,
    merchantId :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Lib.Permit.Domain.Types.Common.MerchantOperatingCity,
    permitDate :: Data.Time.Day,
    permitType :: Lib.Permit.Types.PermitType,
    primaryConflictHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    qrToken :: Kernel.Prelude.Maybe (Kernel.External.Encryption.EncryptedHashedField e Kernel.Prelude.Text),
    referenceNo :: Kernel.Prelude.Text,
    secondaryConflictHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Permit.Domain.Types.PermitApplication.PermitStatus,
    submittedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    validFrom :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type PermitApplication = PermitApplicationE ('AsEncrypted)

type DecryptedPermitApplication = PermitApplicationE ('AsUnencrypted)

instance EncryptedItem PermitApplication where
  type Unencrypted PermitApplication = (DecryptedPermitApplication, HashSalt)
  encryptItem (entity, salt) = do
    details_ <- encryptItem (details entity, salt)
    qrToken_ <- encryptItem $ (,salt) <$> qrToken entity
    pure
      PermitApplication
        { applicantId = applicantId entity,
          approvalLevel = approvalLevel entity,
          consentAt = consentAt entity,
          createdAt = createdAt entity,
          decidedAt = decidedAt entity,
          decidedById = decidedById entity,
          decidedByName = decidedByName entity,
          decisionReason = decisionReason entity,
          details = details_,
          id = id entity,
          merchantId = merchantId entity,
          merchantOperatingCityId = merchantOperatingCityId entity,
          permitDate = permitDate entity,
          permitType = permitType entity,
          primaryConflictHash = primaryConflictHash entity,
          qrToken = qrToken_,
          referenceNo = referenceNo entity,
          secondaryConflictHash = secondaryConflictHash entity,
          status = status entity,
          submittedAt = submittedAt entity,
          updatedAt = updatedAt entity,
          validFrom = validFrom entity,
          validTill = validTill entity
        }
  decryptItem entity = do
    details_ <- fst <$> decryptItem (details entity)
    qrToken_ <- fmap fst <$> decryptItem (qrToken entity)
    pure
      ( PermitApplication
          { applicantId = applicantId entity,
            approvalLevel = approvalLevel entity,
            consentAt = consentAt entity,
            createdAt = createdAt entity,
            decidedAt = decidedAt entity,
            decidedById = decidedById entity,
            decidedByName = decidedByName entity,
            decisionReason = decisionReason entity,
            details = details_,
            id = id entity,
            merchantId = merchantId entity,
            merchantOperatingCityId = merchantOperatingCityId entity,
            permitDate = permitDate entity,
            permitType = permitType entity,
            primaryConflictHash = primaryConflictHash entity,
            qrToken = qrToken_,
            referenceNo = referenceNo entity,
            secondaryConflictHash = secondaryConflictHash entity,
            status = status entity,
            submittedAt = submittedAt entity,
            updatedAt = updatedAt entity,
            validFrom = validFrom entity,
            validTill = validTill entity
          },
        ""
      )

instance EncryptedItem' PermitApplication where
  type UnencryptedItem PermitApplication = DecryptedPermitApplication
  toUnencrypted a salt = (a, salt)
  fromUnencrypted = fst

data ApprovalLevel = STANDARD | DC deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data PermitStatus = DRAFT | SUBMITTED | APPROVED | REJECTED | WITHDRAWN deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ApprovalLevel))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''PermitStatus))

$(mkHttpInstancesForEnum (''PermitStatus))
