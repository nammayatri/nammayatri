{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}

module MerchantDocuments.Tools.Error where

import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError

data MerchantDocumentError
  = MerchantDocumentNotFound Text
  | MerchantDocumentAlreadyExists Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MerchantDocumentError

instance IsBaseError MerchantDocumentError where
  toMessage = \case
    MerchantDocumentNotFound docId -> Just $ "MerchantDocument with id " <> docId <> " not found."
    MerchantDocumentAlreadyExists key -> Just $ "MerchantDocument already exists for " <> key <> "."

instance IsHTTPError MerchantDocumentError where
  toErrorCode (MerchantDocumentNotFound _) = "MERCHANT_DOCUMENT_NOT_FOUND"
  toErrorCode (MerchantDocumentAlreadyExists _) = "MERCHANT_DOCUMENT_ALREADY_EXISTS"
  toHttpCode (MerchantDocumentNotFound _) = E404
  toHttpCode (MerchantDocumentAlreadyExists _) = E400

instance IsAPIError MerchantDocumentError
