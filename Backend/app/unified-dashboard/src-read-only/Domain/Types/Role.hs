{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Role where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Dhall
import qualified Tools.Beam.UtilsTH

data Role = Role
  { createdAt :: Kernel.Prelude.UTCTime,
    description :: Data.Text.Text,
    id :: Kernel.Types.Id.Id Domain.Types.Role.Role,
    name :: Data.Text.Text,
    needsBppAccountCreation :: Kernel.Prelude.Bool,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RoleAPIEntity = RoleAPIEntity {description :: Data.Text.Text, id :: Kernel.Types.Id.Id Domain.Types.Role.Role, name :: Data.Text.Text, needsBppAccountCreation :: Kernel.Prelude.Bool}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
