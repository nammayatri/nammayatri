{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PassDetails where

import qualified Data.Aeson
import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.PassDetails
import qualified Domain.Types.PassType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PassDetailsT f = PassDetailsT
  { address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    age :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    applicableRouteIds :: B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Text]),
    createdAt :: B.C f Data.Time.UTCTime,
    graduationDate :: B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime),
    guardianName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    idCardPicture :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    name :: B.C f Kernel.Prelude.Text,
    numberOfStages :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    passEnum :: B.C f Domain.Types.PassType.PassEnum,
    passOrganizationId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    registerNo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    remark :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    routePairs :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    studentClass :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Data.Time.UTCTime,
    verificationDate :: B.C f (Kernel.Prelude.Maybe Data.Time.UTCTime),
    verificationStatus :: B.C f Domain.Types.PassDetails.VerificationStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PassDetailsT where
  data PrimaryKey PassDetailsT f = PassDetailsId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PassDetailsId <$> id <*> personId

type PassDetails = PassDetailsT Identity

$(enableKVPG ''PassDetailsT ['id, 'personId] [['id], ['passOrganizationId]])

$(mkTableInstances ''PassDetailsT "pass_details")
