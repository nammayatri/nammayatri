{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.DriverReferral
  ( module Dashboard.Common.DriverReferral,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import Data.OpenApi hiding (description, name, password, url)
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Servant

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data ReferralEndpoint
  = ReferralProgramUpdateOpsPasswordEndpoint
  | ReferralProgramLinkEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord)

derivePersistField "ReferralEndpoint"

---------------------------------------------------------
-- merchant driver referral linkage  --------------------

type ReferralProgramPasswordUpdateAPI =
  "referralOpsPassword"
    :> ReqBody '[JSON] ReferralLinkPasswordUpdateAPIReq
    :> Post '[JSON] APISuccess

type ReferralProgramLinkCodeAPI =
  "linkReferral"
    :> MultipartForm Tmp ReferralLinkReq
    :> Post '[JSON] LinkReport

data LinkReport = LinkReport
  { successFullyLinked :: Int,
    failures :: [FailureReasons]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FailureReasons = FailureReasons
  { driverId :: Text,
    failureReason :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype ReferralLinkReq = ReferralLinkReq
  { file :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets ReferralLinkReq where
  hideSecrets = identity

instance FromMultipart Tmp ReferralLinkReq where
  fromMultipart form = do
    ReferralLinkReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp ReferralLinkReq where
  toMultipart uploadFileRequest =
    MultipartData [] [FileData "file" (T.pack uploadFileRequest.file) "" (uploadFileRequest.file)]

newtype ReferralLinkPasswordUpdateAPIReq = ReferralLinkPasswordUpdateAPIReq
  { referralLinkPassword :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- Should we hide password?
instance HideSecrets ReferralLinkPasswordUpdateAPIReq where
  hideSecrets = identity
