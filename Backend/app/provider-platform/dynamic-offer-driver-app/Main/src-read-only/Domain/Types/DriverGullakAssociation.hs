{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DriverGullakAssociation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data DriverGullakAssociation
    = DriverGullakAssociation {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                               gullakToken :: Kernel.Prelude.Text,
                               merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                               merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                               tokenExpiry :: Kernel.Prelude.UTCTime,
                               createdAt :: Kernel.Prelude.UTCTime,
                               updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



