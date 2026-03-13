{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.IffcoTokioInsurance where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.IffcoTokioInsurance
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data IffcoTokioInsuranceT f = IffcoTokioInsuranceT
  { certificateNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    declarationId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    iffcoStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    insuranceStatus :: (B.C f Domain.Types.IffcoTokioInsurance.IffcoTokioInsuranceStatus),
    invoiceRequestNumber :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table IffcoTokioInsuranceT where
  data PrimaryKey IffcoTokioInsuranceT f = IffcoTokioInsuranceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = IffcoTokioInsuranceId . id

type IffcoTokioInsurance = IffcoTokioInsuranceT Identity

$(enableKVPG (''IffcoTokioInsuranceT) [('id)] [[('driverId)], [('invoiceRequestNumber)]])

$(mkTableInstances (''IffcoTokioInsuranceT) "iffco_tokio_insurance")
