module Epass.Constants.APIErrorCode where

import Epass.Types.Common

internalServerErr :: ErrorResponse
internalServerErr =
  ErrorResponse
    { status = "FAILURE",
      responseCode = "INTERNAL_SERVER_ERROR",
      responseMessage = "INTERNAL_SERVER_ERROR"
    }
