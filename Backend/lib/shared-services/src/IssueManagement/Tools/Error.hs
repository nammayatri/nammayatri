{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module IssueManagement.Tools.Error where

import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError

newtype IssueReportError
  = IssueReportDoNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueReportError

instance IsBaseError IssueReportError where
  toMessage = \case
    IssueReportDoNotExist issueReportId -> Just $ "IssueReport with issueReportId \"" <> show issueReportId <> "\" do not exist."

instance IsHTTPError IssueReportError where
  toErrorCode (IssueReportDoNotExist _) = "ISSUE_REPORT_DO_NOT_EXIST"
  toHttpCode (IssueReportDoNotExist _) = E400

instance IsAPIError IssueReportError

data IssueOptionError
  = IssueOptionNotFound Text
  | IssueOptionDoNotExist Text
  | IssueOptionInvalid Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueOptionError

instance IsBaseError IssueOptionError where
  toMessage = \case
    IssueOptionNotFound issueOptionId -> Just $ "IssueOption with issueOptionId \"" <> show issueOptionId <> "\" not found."
    IssueOptionDoNotExist issueOptionId -> Just $ "IssueOption with issueOptionId \"" <> show issueOptionId <> "\" do not exist."
    IssueOptionInvalid issueOptionId issueCategoryId -> Just $ "IssueOption with issueOptionId \"" <> show issueOptionId <> "\" not linked to IssueCategory with issueCategoryId \"" <> show issueCategoryId <> "\"."

instance IsHTTPError IssueOptionError where
  toErrorCode = \case
    IssueOptionNotFound _ -> "ISSUE_OPTION_NOT_FOUND"
    IssueOptionDoNotExist _ -> "ISSUE_OPTION_DO_NOT_EXIST"
    IssueOptionInvalid _ _ -> "ISSUE_OPTION_INVALID"

  toHttpCode = \case
    IssueOptionNotFound _ -> E500
    IssueOptionDoNotExist _ -> E400
    IssueOptionInvalid _ _ -> E400

instance IsAPIError IssueOptionError

data MediaFileError
  = FileSizeExceededError Text
  | FileDoNotExist Text
  | FileFormatNotSupported Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MediaFileError

instance IsHTTPError MediaFileError where
  toErrorCode = \case
    FileSizeExceededError _ -> "FILE_SIZE_EXCEEDED"
    FileDoNotExist _ -> "FILE_DO_NOT_EXIST"
    FileFormatNotSupported _ -> "FILE_FORMAT_NOT_SUPPORTED"
  toHttpCode = \case
    FileSizeExceededError _ -> E400
    FileDoNotExist _ -> E400
    FileFormatNotSupported _ -> E400

instance IsAPIError MediaFileError

instance IsBaseError MediaFileError where
  toMessage = \case
    FileSizeExceededError fileSize -> Just $ "Filesize is " <> fileSize <> " Bytes, which is more than the allowed 10MB limit."
    FileDoNotExist fileId -> Just $ "MediaFile with fileId \"" <> show fileId <> "\" do not exist."
    FileFormatNotSupported fileFormat -> Just $ "MediaFile with fileFormat \"" <> show fileFormat <> "\" not supported."

data IssueCategoryError
  = IssueCategoryNotFound Text
  | IssueCategoryDoNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueCategoryError

instance IsBaseError IssueCategoryError where
  toMessage = \case
    IssueCategoryNotFound issueCategoryId -> Just $ "IssueCategory with issueCategoryId \"" <> show issueCategoryId <> "\" not found."
    IssueCategoryDoNotExist issueCategoryId -> Just $ "IssueCategory with issueCategoryId \"" <> show issueCategoryId <> "\" do not exist."

instance IsHTTPError IssueCategoryError where
  toErrorCode = \case
    IssueCategoryNotFound _ -> "ISSUE_CATEGORY_NOT_FOUND"
    IssueCategoryDoNotExist _ -> "ISSUE_CATEGORY_DO_NOT_EXIST"
  toHttpCode = \case
    IssueCategoryNotFound _ -> E500
    IssueCategoryDoNotExist _ -> E400

instance IsAPIError IssueCategoryError
