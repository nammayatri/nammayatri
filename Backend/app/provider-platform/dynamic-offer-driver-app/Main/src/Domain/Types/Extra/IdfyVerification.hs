{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Extra.IdfyVerification where

import qualified Data.Text as T
import qualified Domain.Types.DocumentVerificationConfig as DVC
import Kernel.Prelude

-- Async doc rows store the DocumentType Show name; sync face-compare audit rows use the faceCompare* tags below.
docTypeToText :: DVC.DocumentType -> Text
docTypeToText = show

parseDocType :: Text -> Maybe DVC.DocumentType
parseDocType = readMaybe . T.unpack

-- | Face-compare audit-row tags; doubles as the hard bound on which documents are face-matched.
faceCompareDocTag :: DVC.DocumentType -> Maybe Text
faceCompareDocTag = \case
  DVC.AadhaarCard -> Just "faceCompareAadhaar"
  DVC.PanCard -> Just "faceComparePan"
  DVC.DriverLicense -> Just "faceCompareDl"
  _ -> Nothing

-- | idfy_verification.status values meaning the async webhook is still awaited.
inProgressIdfyStatuses :: [Text]
inProgressIdfyStatuses = ["pending", "source_down_retrying"]
