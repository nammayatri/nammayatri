{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PassType where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassCategory
import qualified Kernel.Utils.TH
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data PassType
    = PassType {catchline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                id :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                order :: Kernel.Prelude.Int,
                passCategoryId :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory,
                passEnum :: Kernel.Prelude.Maybe Domain.Types.PassType.PassEnum,
                title :: Kernel.Prelude.Text,
                createdAt :: Kernel.Prelude.UTCTime,
                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data PassEnum = TouristPass | RegularPass | StudentPass deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( ToParamSchema))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''PassEnum))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''PassEnum))

