module Engineering.Error.Types.ApiError where

import Prelude

data ApiError
  = NetworkError NetworkErrorType
  | ServerError String
  | ClientError ClientErrorType
  | ParsingError String
  | UnknownError String

data NetworkErrorType = ConnectionRefused | Timeout | OtherNetworkError String

data ClientErrorType = BadRequest | AuthenticationError | AuthorizationError | PathNotFoundError | OtherClientError String
