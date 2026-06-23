{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.RiderPlatform.Management.Rewards
  ( UploadCodesReq (..),
  )
where

import Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.HideSecrets

newtype UploadCodesReq = UploadCodesReq {codesCsv :: FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets UploadCodesReq where
  hideSecrets = identity

instance FromMultipart Tmp UploadCodesReq where
  fromMultipart form =
    UploadCodesReq <$> fmap fdPayload (lookupFile "codesCsv" form)

instance ToMultipart Tmp UploadCodesReq where
  toMultipart form =
    MultipartData [] [FileData "codesCsv" (T.pack form.codesCsv) "text/csv" form.codesCsv]
