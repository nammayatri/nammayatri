{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VendorSplitDetails where

import Data.Aeson
import qualified Data.List as List
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Text.Show
import qualified Tools.Beam.UtilsTH

data VendorSplitDetails = VendorSplitDetails
  { id :: Kernel.Types.Id.Id Domain.Types.VendorSplitDetails.VendorSplitDetails,
    includeInSplit :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    integratedBPPConfigId :: Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig,
    splitShare :: Kernel.Prelude.Maybe Domain.Types.VendorSplitDetails.SplitShare,
    splitType :: Domain.Types.VendorSplitDetails.SplitType,
    vendorId :: Data.Text.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

instance Show SplitShare where
  show (Percentage percentage) = "PERCENTAGE_" <> T.unpack (show percentage)
  show (FixedValue fixedValue) = "FIXEDVALUE_" <> T.unpack (show fixedValue)

data SplitShare = Percentage Kernel.Prelude.Double | FixedValue Kernel.Prelude.Int deriving (Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance Read SplitShare where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (Percentage (read r1 :: Kernel.Prelude.Double), "")
            | r1 <- stripPrefix "PERCENTAGE_" r
          ]
            ++ [ (FixedValue (read r1 :: Kernel.Prelude.Int), "")
                 | r1 <- stripPrefix "FIXEDVALUE_" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

data SplitType = FIXED | FLEXIBLE deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SplitShare)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SplitType)
