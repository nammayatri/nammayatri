module Engineering.Error.Utils.ApiResponseConverter where

import Prelude (($))
import Engineering.Error.Types
import Presto.Core.Types.API (ErrorResponse)

parseApiResponseError :: ErrorResponse -> ApiResponseError
parseApiResponseError apiResponse =
  { code: apiResponse.code
  , responseHeaders: apiResponse.responseHeaders
  , response: 
      { error: apiResponse.response.error
      , errorMessage: parseErrorMessage apiResponse.response.errorMessage
      , userMessage: apiResponse.response.userMessage
      }
  , status: apiResponse.status
  }

class ApiResponseConverter e where
  toApiError :: ApiResponseError -> e -> ApiError

instance defaultApiResponseConverter :: ApiResponseConverter e where
  toApiError resp _ = case resp.code of
    400 -> ClientError $ parse400 resp.response.errorMessage
    401 -> ClientError $ parse401 resp.response.errorMessage
    404 -> ClientError PathNotFoundError
    500 -> ServerError $ case resp.response.errorMessage of
                            ParsedError (ParsedErrorMessage parsed) -> parsed.errorMessage
                            RawError rawMsg -> rawMsg
    _   -> UnknownError $ resp.response.userMessage

parse400 :: ErrorRepresentation -> ClientErrorType
parse400 errMsgRep = 
  case errMsgRep of
    ParsedError (ParsedErrorMessage parsed) ->
      case parsed.errorCode of
        "TOKEN_EXPIRED" -> AuthenticationError
        _ -> BadRequest
    RawError _ -> BadRequest

parse401 :: ErrorRepresentation -> ClientErrorType
parse401 errMsgRep = 
  case errMsgRep of
    ParsedError (ParsedErrorMessage parsed) ->
      case parsed.errorCode of
        "TOKEN_EXPIRED" -> AuthenticationError
        "INVALID_TOKEN" -> AuthenticationError
        _ -> AuthenticationError
    RawError _ -> AuthenticationError
