{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DeriveAnyClass #-}

module IssueManagement.Tools.Error where

import EulerHS.Prelude
import Kernel.Types.Error.BaseError.HTTPError

data IssueReportError
  = IssueReportDoesNotExist Text
  | IssueReportAlreadyExists Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueReportError

instance IsBaseError IssueReportError where
  toMessage = \case
    IssueReportDoesNotExist issueReportId -> Just $ "IssueReport with issueReportId " <> issueReportId <> " does not exist."
    IssueReportAlreadyExists rideId -> Just $ "An issue report already exists for the selected category and rideId-" <> rideId

instance IsHTTPError IssueReportError where
  toErrorCode (IssueReportDoesNotExist _) = "ISSUE_REPORT_DOES_NOT_EXIST"
  toErrorCode (IssueReportAlreadyExists _) = "ISSUE_REPORT_ALREADY_EXISTS"
  toHttpCode (IssueReportDoesNotExist _) = E400
  toHttpCode (IssueReportAlreadyExists _) = E400

instance IsAPIError IssueReportError

data IssueOptionError
  = IssueOptionNotFound Text
  | IssueOptionDoesNotExist Text
  | IssueOptionInvalid Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueOptionError

instance IsBaseError IssueOptionError where
  toMessage = \case
    IssueOptionNotFound issueOptionId -> Just $ "IssueOption with issueOptionId-" <> issueOptionId <> " not found."
    IssueOptionDoesNotExist issueOptionId -> Just $ "IssueOption with issueOptionId-" <> issueOptionId <> " does not exist."
    IssueOptionInvalid issueOptionId issueCategoryId -> Just $ "IssueOption with issueOptionId-" <> issueOptionId <> " not linked to IssueCategory with issueCategoryId " <> show issueCategoryId <> "."

instance IsHTTPError IssueOptionError where
  toErrorCode = \case
    IssueOptionNotFound _ -> "ISSUE_OPTION_NOT_FOUND"
    IssueOptionDoesNotExist _ -> "ISSUE_OPTION_DOES_NOT_EXIST"
    IssueOptionInvalid _ _ -> "ISSUE_OPTION_INVALID"

  toHttpCode = \case
    IssueOptionNotFound _ -> E500
    IssueOptionDoesNotExist _ -> E400
    IssueOptionInvalid _ _ -> E400

instance IsAPIError IssueOptionError

data MediaFileError
  = FileSizeExceededError Text
  | FileDoesNotExist Text
  | FileFormatNotSupported Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''MediaFileError

instance IsHTTPError MediaFileError where
  toErrorCode = \case
    FileSizeExceededError _ -> "FILE_SIZE_EXCEEDED"
    FileDoesNotExist _ -> "FILE_DOES_NOT_EXIST"
    FileFormatNotSupported _ -> "FILE_FORMAT_NOT_SUPPORTED"
  toHttpCode = \case
    FileSizeExceededError _ -> E400
    FileDoesNotExist _ -> E400
    FileFormatNotSupported _ -> E400

instance IsAPIError MediaFileError

instance IsBaseError MediaFileError where
  toMessage = \case
    FileSizeExceededError fileSize -> Just $ "Filesize is " <> fileSize <> " Bytes, which is more than the allowed 10MB limit."
    FileDoesNotExist fileId -> Just $ "MediaFile with fileId-" <> fileId <> " does not exist."
    FileFormatNotSupported fileFormat -> Just $ "MediaFile with fileFormat " <> show fileFormat <> " not supported."

data IssueCategoryError
  = IssueCategoryNotFound Text
  | IssueCategoryDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueCategoryError

instance IsBaseError IssueCategoryError where
  toMessage = \case
    IssueCategoryNotFound issueCategoryId -> Just $ "IssueCategory with issueCategoryId-" <> issueCategoryId <> " not found."
    IssueCategoryDoesNotExist issueCategoryId -> Just $ "IssueCategory with issueCategoryId-" <> issueCategoryId <> " does not exist."

instance IsHTTPError IssueCategoryError where
  toErrorCode = \case
    IssueCategoryNotFound _ -> "ISSUE_CATEGORY_NOT_FOUND"
    IssueCategoryDoesNotExist _ -> "ISSUE_CATEGORY_DOES_NOT_EXIST"
  toHttpCode = \case
    IssueCategoryNotFound _ -> E500
    IssueCategoryDoesNotExist _ -> E400

instance IsAPIError IssueCategoryError

data IssueMessageError
  = IssueMessageNotFound Text
  | IssueMessageDoesNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueMessageError

instance IsBaseError IssueMessageError where
  toMessage = \case
    IssueMessageNotFound issueMessageId -> Just $ "IssueMessage with issueMessageId-" <> issueMessageId <> " not found."
    IssueMessageDoesNotExist issueMessageId -> Just $ "IssueMessage with issueMessageId-" <> issueMessageId <> " does not exist."

instance IsHTTPError IssueMessageError where
  toErrorCode = \case
    IssueMessageNotFound _ -> "ISSUE_MESSAGE_NOT_FOUND"
    IssueMessageDoesNotExist _ -> "ISSUE_MESSAGE_DOES_NOT_EXIST"
  toHttpCode = \case
    IssueMessageNotFound _ -> E500
    IssueMessageDoesNotExist _ -> E400

instance IsAPIError IssueMessageError

newtype IssueConfigError
  = IssueConfigNotFound Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''IssueConfigError

instance IsBaseError IssueConfigError where
  toMessage = \case
    IssueConfigNotFound merchantOpCityId -> Just $ "IssueConfig with merchantOperatingCityId-" <> merchantOpCityId <> " not found."

instance IsHTTPError IssueConfigError where
  toErrorCode = \case
    IssueConfigNotFound _ -> "ISSUE_CONFIG_NOT_FOUND"
  toHttpCode = \case
    IssueConfigNotFound _ -> E500

instance IsAPIError IssueConfigError
