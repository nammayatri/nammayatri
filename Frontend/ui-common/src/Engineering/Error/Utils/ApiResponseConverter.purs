module Engineering.Error.Utils.ApiResponseConverter where

import Prelude (($), (>=) , (&&) , (<=), otherwise ,(<))
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
  toApiError resp _
    | resp.code < 0 = NetworkError $ parseNetworkError resp.response.errorMessage
    | resp.code >= 400 && resp.code <= 463 = ClientError $ parseClientError resp.response.errorMessage
    | resp.code >= 500 && resp.code <= 543 = ServerError $ parseServerError resp.response.errorMessage
    | otherwise = UnknownError $ resp.response.userMessage



parseClientError :: ErrorRepresentation -> ClientErrorType
parseClientError errMsgRep = 
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

parseNetworkError :: ErrorRepresentation -> NetworkErrorType
parseNetworkError errMsgRep =
  case errMsgRep of
    ParsedError (ParsedErrorMessage parsed) ->
      case parsed.errorCode of
        "CONNECTION_REFUSED" -> ConnectionRefused
        "TIMEOUT" -> Timeout
        _ -> OtherNetworkError parsed.errorMessage
    RawError rawMsg -> OtherNetworkError rawMsg

parseServerError :: ErrorRepresentation -> String
parseServerError errMsgRep =
  case errMsgRep of
    ParsedError (ParsedErrorMessage parsed) -> parsed.errorMessage
    RawError rawMsg -> rawMsg