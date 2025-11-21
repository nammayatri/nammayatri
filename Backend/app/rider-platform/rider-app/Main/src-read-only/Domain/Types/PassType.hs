{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PassType where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PassCategory
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data PassType = PassType
  { catchline :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    code :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    description :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassType.PassType,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    order :: Kernel.Prelude.Int,
    passCategoryId :: Kernel.Types.Id.Id Domain.Types.PassCategory.PassCategory,
    passClass :: Domain.Types.PassType.PassClass,
    title :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PassClass = Unlimited | StoredValuePass deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Enum, Bounded)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''PassClass)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''PassClass)
