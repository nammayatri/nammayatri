{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FlaggedCategory where

import Kernel.Prelude
import qualified Kernel.Types.Id

data FlaggedCategory = FlaggedCategory {createdAt :: Kernel.Prelude.UTCTime, id :: Kernel.Types.Id.Id Domain.Types.FlaggedCategory.FlaggedCategory, name :: Kernel.Prelude.Text, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
