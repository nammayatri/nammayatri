{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.VendorFee where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.DriverFee
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data VendorFee
    = VendorFee {amount :: Kernel.Types.Common.HighPrecMoney,
                 driverFeeId :: Kernel.Types.Id.Id Domain.Types.DriverFee.DriverFee,
                 vendorId :: Data.Text.Text,
                 createdAt :: Kernel.Prelude.UTCTime,
                 updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



