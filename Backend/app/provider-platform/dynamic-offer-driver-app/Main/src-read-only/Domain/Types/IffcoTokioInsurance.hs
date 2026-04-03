{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.IffcoTokioInsurance where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data IffcoTokioInsurance
    = IffcoTokioInsurance {certificateNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           createdAt :: Kernel.Prelude.UTCTime,
                           declarationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                           id :: Kernel.Types.Id.Id Domain.Types.IffcoTokioInsurance.IffcoTokioInsurance,
                           iffcoStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           insuranceStatus :: Domain.Types.IffcoTokioInsurance.IffcoTokioInsuranceStatus,
                           invoiceRequestNumber :: Kernel.Prelude.Text,
                           merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                           updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( Eq))
data IffcoTokioInsuranceStatus = PENDING | INSURED | FAILED deriving (Read, ( Show), ( Eq), ( Generic), ( FromJSON), ( ToJSON), ( ToSchema), ( ToParamSchema), ( Ord))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''IffcoTokioInsuranceStatus))

