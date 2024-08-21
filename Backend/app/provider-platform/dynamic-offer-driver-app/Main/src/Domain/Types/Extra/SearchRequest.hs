{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.SearchRequest where

import Data.Aeson
import Domain.Types.SearchRequestDeliveryDetails
import Kernel.Prelude
import Kernel.Utils.TH
import qualified Text.Show

-- Extra code goes here --
instance Show SearchRequestDeliveryDetails where
  show (SearchRequestDeliveryDetails {..}) =
    "SearchRequestDeliveryDetails { initiatedAs = " ++ show initiatedAs ++ ", receiverDetails = " ++ show receiverDetails ++ ", searchRequestId = " ++ show searchRequestId ++ ", senderDetails = " ++ show senderDetails ++ ", createdAt = " ++ show createdAt ++ ", updatedAt = " ++ show updatedAt ++ " }"
