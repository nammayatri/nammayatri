{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MediaFileDocument where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.VehicleRegistrationCertificate
import qualified Tools.Beam.UtilsTH



data MediaFileDocument
    = MediaFileDocument {creatorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                         fileHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         id :: Kernel.Types.Id.Id Domain.Types.MediaFileDocument.MediaFileDocument,
                         mediaFileDocumentType :: Domain.Types.Common.MediaFileDocumentType,
                         merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                         rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
                         s3Path :: Kernel.Prelude.Text,
                         status :: Domain.Types.MediaFileDocument.MediaFileDocumentStatus,
                         uploadLink :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                         createdAt :: Kernel.Prelude.UTCTime,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data MediaFileDocumentStatus = PENDING | DELETED | FAILED | CONFIRMED | COMPLETED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MediaFileDocumentStatus))

