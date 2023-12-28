module Engineering.Error.Utils (
  module Error, 
  handleApiError
) where

import Prelude
import Engineering.Error.Types as Error
import Engineering.Error.Utils.ApiErrorHandler as Error
import Engineering.Error.Utils.ApiResponseConverter as Error
import Presto.Core.Types.API (ErrorResponse)
import Common.Types.App (FlowBT)

-- Different errors
-- 1. Invalid Url: Compilation should fail instead of throwing error to user
-- 2. Network Error (Connection Refused, Timeout, etc) [do a retry mechanism for this]
-- 3. Server Error (5XX) [Show error message to user in popup or log the error]
-- 4. Client Error (4XX) [Authentication Error, Authorization Error, Path Not found etc] (specific action on basis on error type)
-- 5. Parsing Error (JSON parsing error, etc) (set nothing for the fields for which parsing failed with proper error log)
-- 6. Unknown Error (Any other error) (log the error)

handleApiError :: forall e st. Error.ApiErrorHandler e st => e -> ErrorResponse -> FlowBT String st Unit
handleApiError errorHandler err = do
  let apiError = Error.toApiError (Error.parseApiResponseError err) errorHandler
  Error.handleAllErrors errorHandler
  case apiError of
    Error.NetworkError netErr -> Error.handleNetworkError netErr errorHandler
    Error.ServerError msg -> Error.handleServerError msg errorHandler
    Error.ClientError clientErr -> Error.handleClientError clientErr errorHandler
    Error.ParsingError msg -> Error.handleParsingError msg errorHandler
    Error.UnknownError msg -> Error.handleUnknownError msg errorHandler
