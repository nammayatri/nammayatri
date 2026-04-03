{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DriverRCAssociation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data DriverRCAssociation
    = DriverRCAssociation {associatedOn :: Kernel.Prelude.UTCTime,
                           associatedTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                           consent :: Kernel.Prelude.Bool,
                           consentTimestamp :: Kernel.Prelude.UTCTime,
                           driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                           errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                           id :: Kernel.Types.Id.Id Domain.Types.DriverRCAssociation.DriverRCAssociation,
                           isRcActive :: Kernel.Prelude.Bool,
                           rcId :: Kernel.Types.Id.Id Domain.Types.VehicleRegistrationCertificate.VehicleRegistrationCertificate,
                           merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                           merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                           createdAt :: Kernel.Prelude.UTCTime,
                           updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



