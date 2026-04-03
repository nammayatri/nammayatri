{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BlockedRoute where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Utils.ComputeIntersection
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data BlockedRoute
    = BlockedRoute {createdAt :: Kernel.Prelude.UTCTime,
                    enabled :: Kernel.Prelude.Bool,
                    endSegment :: Kernel.Utils.ComputeIntersection.LineSegment,
                    id :: Kernel.Types.Id.Id Domain.Types.BlockedRoute.BlockedRoute,
                    name :: Kernel.Prelude.Text,
                    startSegment :: Kernel.Utils.ComputeIntersection.LineSegment,
                    updatedAt :: Kernel.Prelude.UTCTime,
                    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)}
    deriving (Generic, Show, Read, FromJSON, ToJSON)



