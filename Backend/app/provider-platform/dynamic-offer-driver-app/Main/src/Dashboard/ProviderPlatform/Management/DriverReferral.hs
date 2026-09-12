{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dashboard.ProviderPlatform.Management.DriverReferral where

import API.Types.ProviderPlatform.Management.Endpoints.DriverReferral as Reexport
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.ServantMultipart

instance FromMultipart Tmp ReferralLinkReq where
  fromMultipart form = do
    ReferralLinkReq
      <$> fmap fdPayload (lookupFile "file" form)

instance ToMultipart Tmp ReferralLinkReq where
  toMultipart uploadFileRequest =
    MultipartData [] [FileData "file" (T.pack uploadFileRequest.file) "" (uploadFileRequest.file)]
