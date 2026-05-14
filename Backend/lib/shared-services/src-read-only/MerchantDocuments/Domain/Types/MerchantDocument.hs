{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MerchantDocuments.Domain.Types.MerchantDocument where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified MerchantDocuments.Domain.Types.Common
import qualified Tools.Beam.UtilsTH

data MerchantDocument = MerchantDocument
  { createdAt :: Kernel.Prelude.UTCTime,
    documentType :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id MerchantDocuments.Domain.Types.MerchantDocument.MerchantDocument,
    language :: Kernel.External.Types.Language,
    merchantId :: Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.Merchant,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id MerchantDocuments.Domain.Types.Common.MerchantOperatingCity),
    platformType :: Kernel.Prelude.Maybe MerchantDocuments.Domain.Types.MerchantDocument.PlatformType,
    role :: MerchantDocuments.Domain.Types.MerchantDocument.Role,
    title :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    url :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PlatformType = WEB | ANDROID | IOS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data Role = Driver | FleetOwner | Operator | Rider | Admin deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''PlatformType)

$(mkHttpInstancesForEnum ''PlatformType)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''Role)

$(mkHttpInstancesForEnum ''Role)
