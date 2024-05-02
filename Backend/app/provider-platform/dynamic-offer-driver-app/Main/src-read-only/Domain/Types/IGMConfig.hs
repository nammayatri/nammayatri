{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.IGMConfig where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data IGMConfig = IGMConfig
  { expectedResolutionTime :: Kernel.Prelude.Int,
    expectedResponseTime :: Kernel.Prelude.Int,
    groEmail :: Data.Text.Text,
    groName :: Data.Text.Text,
    groPhone :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.IGMConfig.IGMConfig,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
