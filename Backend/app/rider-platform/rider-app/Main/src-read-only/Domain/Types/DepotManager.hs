{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.DepotManager where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Depot
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data DepotManager
    = DepotManager {createdAt :: Kernel.Prelude.UTCTime,
                    depotCode :: Kernel.Types.Id.Id Domain.Types.Depot.Depot,
                    enabled :: Kernel.Prelude.Bool,
                    isAdmin :: Kernel.Prelude.Bool,
                    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



