{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Yudhishthira.Tools.Error
  ( module Lib.Yudhishthira.Tools.Error,
    module Error,
  )
where

import Data.Text as T
import Kernel.Prelude
import Kernel.Types.Error as Error hiding (MerchantError)
import Kernel.Types.Error.BaseError.HTTPError.HttpCode
import Kernel.Utils.Common hiding (Error)

data Error
  = TagNotFound Text
  | TagAlreadyExists Text
  | RepeatedQueryFields [Text]
  | MissingQueryFields [Text]
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''Error

instance IsBaseError Error where
  toMessage = \case
    TagNotFound name -> Just $ "Tag not found: " <> name
    TagAlreadyExists name -> Just $ "Tag already exists: " <> name
    RepeatedQueryFields fields -> Just $ "Repeated query fields: " <> T.intercalate ", " fields
    MissingQueryFields fields -> Just $ "Missing query fields: " <> T.intercalate ", " fields

instance IsHTTPError Error where
  toErrorCode = \case
    TagNotFound _ -> "TAG_NOT_FOUND"
    TagAlreadyExists _ -> "TAG_ALREADY_EXISTS"
    RepeatedQueryFields _ -> "REPEATED_QUERY_FIELDS"
    MissingQueryFields _ -> "MISSING_QUERY_FIELDS"

  toHttpCode = \case
    TagNotFound _ -> E400
    TagAlreadyExists _ -> E400
    RepeatedQueryFields _ -> E400
    MissingQueryFields _ -> E400

instance IsAPIError Error

data ChakraQueriesError
  = ChakraQueriesAlreadyExists Text Text
  | ChakraQueriesDoesNotExist Text Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ChakraQueriesError

instance IsBaseError ChakraQueriesError where
  toMessage = \case
    ChakraQueriesAlreadyExists chakra queryName -> Just $ "Chakra query with chakra \"" <> chakra <> "\" and queryName \"" <> queryName <> "\" already exist"
    ChakraQueriesDoesNotExist chakra queryName -> Just $ "Chakra query with chakra \"" <> chakra <> "\" and queryName \"" <> queryName <> "\" does not exist"

instance IsHTTPError ChakraQueriesError where
  toErrorCode = \case
    ChakraQueriesAlreadyExists _ _ -> "CHAKRA_QUERIES_ALREADY_EXISTS"
    ChakraQueriesDoesNotExist _ _ -> "CHAKRA_QUERIES_DOES_NOT_EXIST"

  toHttpCode = \case
    ChakraQueriesAlreadyExists _ _ -> E400
    ChakraQueriesDoesNotExist _ _ -> E400

instance IsAPIError ChakraQueriesError
