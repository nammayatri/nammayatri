{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BapMetadata where

import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data BapMetadata = BapMetadata
  { id :: Kernel.Types.Id.Id Domain.Types.BapMetadata.BapMetadata,
    logoUrl :: Servant.Client.Core.BaseUrl,
    name :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
