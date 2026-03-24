{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DeletedPerson where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Version
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data DeletedPerson
    = DeletedPerson {clientOsType :: Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType,
                     createdAt :: Kernel.Prelude.UTCTime,
                     deviceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                     merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                     personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                     reasonToDelete :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                     updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



