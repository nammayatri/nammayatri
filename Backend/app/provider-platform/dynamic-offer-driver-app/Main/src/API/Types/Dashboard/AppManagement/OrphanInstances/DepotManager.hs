{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types.Dashboard.AppManagement.OrphanInstances.DepotManager
  ( module ReExport,
    maskMobile,
  )
where

import API.Types.Dashboard.AppManagement.Endpoints.DepotManager as ReExport
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Types.HideSecrets (HideSecrets (..))

maskMobile :: Text -> Text
maskMobile mn
  | T.length mn <= 4 = T.replicate (T.length mn) "*"
  | otherwise = T.replicate (T.length mn - 4) "*" <> T.takeEnd 4 mn

instance HideSecrets DepotManagerDetail where
  type ReqWithoutSecrets DepotManagerDetail = DepotManagerDetail
  hideSecrets DepotManagerDetail {..} =
    DepotManagerDetail
      { mobileNumber = maskMobile mobileNumber,
        ..
      }

instance HideSecrets DepotManagerDetails where
  type ReqWithoutSecrets DepotManagerDetails = DepotManagerDetails
  hideSecrets (DepotManagerDetails items) = DepotManagerDetails (map hideSecrets items)
