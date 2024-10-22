{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.FlaggedCategory where

import qualified API.Types.UI.Notification
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.FlaggedCategory
import EulerHS.Prelude hiding (id)
import Servant hiding (Summary)
import "lib-dashboard" Tools.Auth

data AddFlagCategoryReq = AddFlagCategoryReq {name :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeleteFlagCategoryReq = DeleteFlagCategoryReq {id :: Data.Text.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FlagCategoryList = FlagCategoryList {flagCategoryList :: [Domain.Types.FlaggedCategory.FlaggedCategory], summary :: API.Types.UI.Notification.Summary}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
