{-# LANGUAGE RecordWildCards #-}

module Errors.PredefinedErrors where

import qualified Data.Aeson.Encode.Pretty as A (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text, pack)
-- TODO: This module is screwy vis a vis imports. Either qualify it or fix it. -
-- Koz
import Errors.Types
import EulerHS.Extra.AltValidation as V
import Prelude

-- Predefined Errors from src/Engineering/Flow.purs

------------------------------------------------------------------------------
-- Errors without arguments
------------------------------------------------------------------------------

mandatoryFieldsAreMissing :: ECErrorPayload
mandatoryFieldsAreMissing =
  ECErrorPayload
    { status = "Bad Request",
      status_id = Nothing,
      error_message = Just "Mandatory fields are missing",
      error_code = Just "Mandatory fields are missing"
    }

mandateMaxAmountIsMissing :: ECErrorPayload
mandateMaxAmountIsMissing =
  ECErrorPayload
    { status = "ERROR",
      status_id = Just (-1),
      --  , error_message = Just $ "Pass mandate_max_amount also to create a mandate order"
      error_message = Just "mandate_max_amount is not passed or is invalid",
      error_code = Just "INVALID_REQUEST"
    }

customerIdNotPresent :: ECErrorResponse
customerIdNotPresent =
  ECErrorResponse
    { code = 400,
      response = A.encodePretty invalidCustomerId
    }

ipBadOrigin :: ErrorResponse
ipBadOrigin =
  ErrorResponse
    { code = 403,
      response =
        ErrorPayload
          { errorMessage = "Bad origin.",
            userMessage = "Ip address is bad",
            error = True
          }
    }

merchantIdError :: ErrorResponse
merchantIdError =
  ErrorResponse
    { code = 500,
      response =
        ErrorPayload
          { errorMessage = "Server error.",
            userMessage = "Invalid merchant id. Cannot process your request.",
            error = True
          }
    }

customerIdError :: ErrorResponse
customerIdError =
  ErrorResponse
    { code = 500,
      response =
        ErrorPayload
          { errorMessage = "Server error.",
            userMessage = "Invalid customer id. Cannot process your request.",
            error = True
          }
    }

resellerIdError :: ErrorResponse
resellerIdError =
  ErrorResponse
    { code = 500,
      response =
        ErrorPayload
          { errorMessage = "Server error.",
            userMessage = "Invalid reseller id. Cannot process your request.",
            error = True
          }
    }

loginContextError :: ErrorResponse
loginContextError =
  ErrorResponse
    { code = 403,
      response =
        ErrorPayload
          { errorMessage = "Forbidden request.",
            userMessage = "Unauthorized user role.",
            error = True
          }
    }

twoFAError :: ErrorResponse
twoFAError =
  ErrorResponse
    { code = 401,
      response =
        ErrorPayload
          { errorMessage = "TOTP enabled for user.",
            userMessage = "TOTP is enabled for user, provide OTP and try again.",
            error = True
          }
    }

otpError :: ErrorResponse
otpError =
  ErrorResponse
    { code = 401,
      response =
        ErrorPayload
          { errorMessage = "Unauthorized.",
            userMessage = "Invalid OTP. Please try again.",
            error = True
          }
    }

accessDenied :: ErrorResponse
accessDenied =
  ErrorResponse
    { code = 401,
      response =
        ErrorPayload
          { errorMessage = "Access Denied",
            userMessage = "Access Denied",
            error = True
          }
    }

internalError :: ErrorResponse
internalError =
  ErrorResponse
    { code = 500,
      response =
        ErrorPayload
          { errorMessage = "Internal Server Error",
            userMessage = "Internal Server Error",
            error = True
          }
    }

sqlInjectionError :: ErrorResponse
sqlInjectionError =
  ErrorResponse
    { code = 500,
      response =
        ErrorPayload
          { errorMessage = "SQL Injection Attempted",
            userMessage = "SQL Injection Attempted",
            error = True
          }
    }

timeLimitExceeded :: ErrorResponse
timeLimitExceeded =
  ErrorResponse
    { code = 500,
      response =
        ErrorPayload
          { errorMessage = "Permitted Time Range Exceeded",
            userMessage = "Permitted Time Range Exceeded",
            error = True
          }
    }

mandateNotFound :: ErrorResponse
mandateNotFound =
  ErrorResponse
    { code = 400,
      response =
        ErrorPayload
          { errorMessage = "Mandate not found",
            userMessage = "Mandate not found",
            error = True
          }
    }

invalidOrderId :: ErrorResponse
invalidOrderId =
  ErrorResponse
    { code = 400,
      response =
        ErrorPayload
          { errorMessage = "Invalid order id.",
            userMessage = "Invalid order id.",
            error = True
          }
    }

invalidMandateParams :: ECErrorResponse
invalidMandateParams =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Just (-1),
              error_message = Just "invalid mandate params",
              error_code = Just "INVALID_REQUEST"
            }
    }

-- | Wrong order amount in request
-- FIXME used for wrong mandate_max_limit_amount as well for some reasons
invalidOrderAmount :: ECErrorPayload
invalidOrderAmount =
  ECErrorPayload
    { status = "ERROR",
      status_id = Nothing,
      error_message = Just "Invalid amount",
      error_code = Just "invalid_request"
    }

unknown400p :: ECErrorPayload
unknown400p =
  ECErrorPayload
    { status = "invalid_request_error",
      status_id = Nothing,
      error_message = Just "unknown request validation error",
      error_code = Just "INVALID_REQUEST"
    }

unknown400 :: ECErrorResponse
unknown400 =
  ECErrorResponse
    { code = 400,
      response = A.encodePretty unknown400p
    }

invalidCustomerId :: ECErrorPayload
invalidCustomerId =
  ECErrorPayload
    { status = "ERROR",
      status_id = Just (-1),
      error_message = Just "pass customer_id",
      error_code = Just "INVALID_CUSTOMER_ID"
    }

orderIdIsMissing :: ECErrorPayload
orderIdIsMissing =
  ECErrorPayload
    { status = "Bad Request",
      status_id = Nothing,
      error_message = Just "order_id is missing",
      error_code = Just "order_id is missing"
    }

txnCardInfoValidationError :: ECErrorResponse
txnCardInfoValidationError =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_response_error",
              status_id = Nothing,
              error_message = Just "TxnCardInfo validation failed",
              error_code = Nothing
            }
    }

ecBadOrigin :: ECErrorResponse
ecBadOrigin =
  ECErrorResponse
    { code = 403,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "bad_origin",
              status_id = Nothing,
              error_message = Just "Bad Origin. Permission denied.",
              error_code = Just "bad_origin"
            }
    }

--ecForbidden :: ECErrorResponse
--ecForbidden =ECErrorResponse
--  { code = 403
--  , response = A.encodePretty $ ECErrorPayload
--      { status = "Forbidden Resource"
--      , status_id = Nothing
--      , error_message = Just "Authentication is required for fetching order details"
--      , error_code = Just "forbidden_resource"
--      }
--  }

ecAccessDenied :: ECErrorResponse
ecAccessDenied =
  ECErrorResponse
    { code = 401,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "error",
              status_id = Nothing,
              error_message = Nothing,
              error_code = Just "access_denied"
            }
    }

-- EHS FIXME use with #382
ecMandatoryFieldsMissing :: ECErrorResponse
ecMandatoryFieldsMissing =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "error",
              status_id = Nothing,
              error_message = Just "Mandatory fields are missing",
              error_code = Just "Mandatory fields are missing"
            }
    }

--txnNotProcessError :: ECErrorResponse
--txnNotProcessError = ECErrorResponse
--  { code = 400
--  , response = A.encodePretty $ ECErrorPayload
--      { status = "invalid_request_error"
--      , status_id = Nothing
--      , error_message = Just "Transaction is inprocess"
--      , error_code = Just "invalid"
--      }
--  }

badRequest :: ECErrorResponse
badRequest =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "error",
              status_id = Nothing,
              error_message = Nothing,
              error_code = Just "invalid_request"
            }
    }

--merchantIdMissing :: ECErrorResponse
--merchantIdMissing = ECErrorResponse
--  { code = 400
--  , response = A.encodePretty $ ECErrorPayload
--      { status = "invalid_request_error"
--      , status_id = Nothing
--      , error_message = Just "Merchant ID information is missing. Either authenticate or pass merchant_id."
--      , error_code = Just "missing"
--      }
--  }

merchantAccountNull :: ECErrorResponse
merchantAccountNull =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just "[merchantAccount] cannot be null",
              error_code = Just "nullable"
            }
    }

customerNotFound :: ECErrorResponse
customerNotFound =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "NOT_FOUND",
              status_id = Just 40,
              error_message = Just "Customer not found for this order",
              error_code = Nothing
            }
    }

customerAlreadyExist :: ECErrorResponse
customerAlreadyExist =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just "Customer with given object reference id already exist for your account.",
              error_code = Just "unique"
            }
    }

bankAccountNotFound :: ECErrorResponse
bankAccountNotFound =
  ECErrorResponse
    { code = 400, -- This is right, since request is "valid", just no customer with specified id was found
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "error",
              status_id = Nothing,
              error_message = Just "Bank account is not found",
              error_code = Just "invalid_request"
            }
    }

bankAccountIdNotFound :: ECErrorResponse
bankAccountIdNotFound =
  ECErrorResponse
    { code = 400, -- This is right, since request is "valid", just no customer with specified id was found
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Nothing,
              error_message = Just "Bank account id is not found in route params",
              error_code = Just "INVALID_REQUEST"
            }
    }

bankAccountForCustomerNotFound :: ECErrorResponse
bankAccountForCustomerNotFound =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "internal_error",
              status_id = Nothing,
              error_message = Just "Bank account is not found for this customer",
              error_code = Nothing
            }
    }

juspayBankCodeNotFound :: ECErrorResponse
juspayBankCodeNotFound =
  ECErrorResponse
    { code = 500, -- This is right, since request is "valid", just no customer with specified id was found
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "internal_error",
              status_id = Nothing,
              error_message = Just "No suitable juspay bank code found.",
              error_code = Nothing
            }
    }

merchantGatewayAccountNotFound :: ECErrorResponse
merchantGatewayAccountNotFound =
  ECErrorResponse
    { code = 500, -- This is right, since request is "valid", just no customer with specified id was found
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "internal_error",
              status_id = Nothing,
              error_message = Just "Gateway account not found",
              error_code = Nothing
            }
    }

merchantGatewayAccountIdNothing :: ECErrorResponse
merchantGatewayAccountIdNothing =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Nothing,
              error_message = Just "Merchant gateway account Id is Nothing",
              error_code = Just "INVALID_REQUEST"
            }
    }

customerIdNotFound :: ECErrorResponse
customerIdNotFound =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Just 40,
              error_message = Just "CustomerId not found at route parameters",
              error_code = Just "INVALID_REQUEST"
            }
    }

orderIdUriBodyMismatch :: ECErrorResponse
orderIdUriBodyMismatch =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "INVALID_REQUEST",
              status_id = Nothing,
              error_message = Just "order_id in URI should be the same as order_id in the body",
              error_code = Nothing
            }
    }

orderIdNotFoundInPath :: ECErrorResponse
orderIdNotFoundInPath =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "Bad Request",
              status_id = Nothing,
              error_message = Just "order_id is missing",
              error_code = Just "order_id is missing"
            }
    }

unAuthorizedAccess :: ECErrorResponse
unAuthorizedAccess =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "Unauthorized access",
              status_id = Nothing,
              error_message = Nothing,
              error_code = Nothing
            }
    }

cardNotFound :: ECErrorResponse
cardNotFound =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just "No card could be located for token",
              error_code = Nothing
            }
    }

secondFactorNotFound :: ECErrorResponse
secondFactorNotFound =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just "Second Factor not found for this transaction id",
              error_code = Nothing
            }
    }

authAccntNotFound :: ECErrorResponse
authAccntNotFound =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "authentication_account_not_found",
              status_id = Nothing,
              error_message = Just "Authentication Account not found",
              error_code = Nothing
            }
    }

sessionTimeout :: ECErrorResponse
sessionTimeout =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "session_expried",
              status_id = Nothing,
              error_message = Just "Session Expired",
              error_code = Nothing
            }
    }

sessionIdNotFound :: ECErrorResponse
sessionIdNotFound =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "Bad Request",
              status_id = Nothing,
              error_message = Just "session id not found in route parameters",
              error_code = Just "session id is missing"
            }
    }

alreadyDeletedEntity :: ECErrorResponse
alreadyDeletedEntity =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just "This entity is already deleted.",
              error_code = Just "already_deleted"
            }
    }

customerIdUriBodyMismatch :: ECErrorResponse
customerIdUriBodyMismatch =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "INVALID_REQUEST",
              status_id = Nothing,
              error_message = Just "customer_id in URI should be the same as customer_id in the body",
              error_code = Nothing
            }
    }

customerIdNotFoundInPath :: ECErrorResponse
customerIdNotFoundInPath =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "Bad Request",
              status_id = Nothing,
              error_message = Just "customer_id is missing",
              error_code = Just "customer_id is missing"
            }
    }

viewValueEmpty :: ECErrorResponse
viewValueEmpty =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "Empty value",
              status_id = Nothing,
              error_message = Just "Failed to review/preview",
              error_code = Just "Text value is empty"
            }
    }

failedViewValue :: ECErrorResponse
failedViewValue =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "View value with prism is failed",
              status_id = Nothing,
              error_message = Just "Can not to review or preview value",
              error_code = Nothing
            }
    }

------------------------------------------------------------------------------
-- Errors with arguments
------------------------------------------------------------------------------

emandateDetailsDecodeError :: Text -> Text -> ECErrorResponse
emandateDetailsDecodeError rawEmandateDetail orderId =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECOrderStatusErrorPayload
            { status = "internal_error",
              status_id = Nothing,
              error_message = Just ("can't decode paymentSource: " <> rawEmandateDetail),
              error_code = Nothing,
              order_id = Just orderId
            }
    }

customErrorResponse :: Int -> Text -> Text -> ErrorResponse
customErrorResponse kode errMsg userMsg =
  ErrorResponse
    { code = kode,
      response =
        ErrorPayload
          { errorMessage = errMsg,
            userMessage = userMsg,
            error = True
          }
    }

toECErrorResponse500 :: BSL.ByteString -> ECErrorResponse
toECErrorResponse500 err =
  ECErrorResponse
    { code = 500,
      response = err
    }

customECErrorResponse :: Int -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> ECErrorResponse
customECErrorResponse kode stat errCode msg statId =
  ECErrorResponse
    { code = kode,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = stat,
              error_code = errCode,
              error_message = msg,
              status_id = statId
            }
    }

networkNotSupported :: Text -> ECErrorResponse
networkNotSupported err =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "network_not_supported",
              status_id = Nothing,
              error_message = Just $ "Network not supported " <> err,
              error_code = Nothing
            }
    }

paymentTypeNotSupported :: Text -> ECErrorResponse
paymentTypeNotSupported pm =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "payment_type_not_supported",
              status_id = Nothing,
              error_message = Just $ "Payment type not supported" <> pm,
              error_code = Nothing
            }
    }

authTypeNotSupported :: Text -> ECErrorResponse
authTypeNotSupported auth_type =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "auth_type_not_supported",
              status_id = Nothing,
              error_message = Just $ "Auth type not supported" <> auth_type,
              error_code = Nothing
            }
    }

generic500 :: Text -> ECErrorResponse
generic500 errMsg =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "internal_error",
              status_id = Nothing,
              error_message = Just errMsg,
              error_code = Nothing
            }
    }

txnOfferNotFound :: Text -> ECErrorResponse
txnOfferNotFound txnId =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just ("txnOffer missing for txnid: " <> txnId),
              error_code = Nothing
            }
    }

gatewayNotSupported :: Text -> ECErrorResponse
gatewayNotSupported gwStr =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "gateway_not_supported",
              status_id = Nothing,
              error_message = Just $ "Gateway not supported" <> gwStr,
              error_code = Nothing
            }
    }

customerNotFoundWithId :: Text -> ECErrorResponse
customerNotFoundWithId customerId =
  ECErrorResponse
    { code = 200, -- This is right, since request is "valid", just no customer with specified id was found
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Just 40,
              error_message = Just $ "Customer with customerId: " <> customerId <> " not found",
              -- ESH TODO This doesn't go well together with Status 200 OK?
              error_code = Just "INVALID_REQUEST"
            }
    }

walletNotFoundWithId :: Text -> ECErrorResponse
walletNotFoundWithId walletId =
  ECErrorResponse
    { code = 400, -- This is right, since request is "valid", just no customer with specified id was found
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Nothing,
              error_message = Just $ "WalletAccount with walletId: " <> walletId <> " not found",
              -- ESH TODO This doesn't go well together with Status 200 OK?
              error_code = Just "INVALID_REQUEST"
            }
    }

pmNotFound :: Text -> ECErrorResponse
pmNotFound txnId =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just ("txnCardInfo missing for txnid: " <> txnId),
              error_code = Nothing
            }
    }

-- EHS: looks as if id, not orderId was in the original author's mind here?
orderDoesNotExist :: Text -> ECErrorResponse
orderDoesNotExist orderId =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              error_code = Nothing,
              error_message = Just ("Order with id = \"" <> orderId <> "\" does not exist."),
              status_id = Nothing
            }
    }

-- EHS: again, is `id`, not `orderId` expected here? Otherwise we are loosing order identification here
orderNotFound :: Text -> ECErrorResponse
orderNotFound orderId =
  ECErrorResponse
    { code = 200,
      response =
        A.encodePretty $
          ECOrderStatusErrorPayload
            { status = "NOT_FOUND",
              status_id = Just 40,
              error_message = Nothing,
              error_code = Nothing,
              order_id = Just orderId
            }
    }

orderAlreadyCreated :: Text -> ECErrorResponse
orderAlreadyCreated orderId =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECOrderStatusErrorPayload
            { status = "order_already_created",
              status_id = Nothing,
              error_message = Just ("Order with id = \"" <> orderId <> "\" already created."),
              error_code = Nothing,
              order_id = Just orderId
            }
    }

txnNotFound :: Text -> ECErrorResponse
txnNotFound tid =
  ECErrorResponse
    { code = 200,
      response =
        A.encodePretty $
          ECTxnStatusErrorPayload
            { status = "NOT_FOUND",
              status_id = Just 40,
              error_message = Nothing,
              error_code = Nothing,
              txn_uuid = Just tid
            }
    }

badSignatureRequest :: Text -> ECErrorResponse
badSignatureRequest field =
  ECErrorResponse
    { code = 401,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "error",
              status_id = Nothing,
              error_message = Just $ field <> " not found in request",
              error_code = Just "invalid_request"
            }
    }

genericBadRequest :: Text -> ECErrorResponse
genericBadRequest errMsg =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "error",
              status_id = Nothing,
              error_message = Just errMsg,
              error_code = Just "invalid_request"
            }
    }

txnValidationErrorResp :: TxnValidationErrorResp -> ECErrorResponse
txnValidationErrorResp TxnValidationErrorResp {..} =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "invalid_request_error",
              status_id = Nothing,
              error_message = Just error_message,
              error_code = Just error_code
            }
    }

mkValidationError :: [Text] -> ECErrorResponse
mkValidationError errs =
  ECErrorResponse
    { code = 400,
      response = A.encodePretty (unknown400p {error_message = Just . pack $ show errs} :: ECErrorPayload)
    }

mkDBError :: Text -> ECErrorResponse
mkDBError err =
  ECErrorResponse
    { code = 500,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "INTERNAL_ERROR",
              status_id = Nothing,
              error_message = Just err,
              error_code = Just "INTERNAL_ERROR"
            }
    }

mkECError :: V.VErrorPayload -> ECErrorResponse
mkECError vErr =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = V.status vErr,
              status_id = V.status_id vErr,
              error_message = V.error_message vErr, -- foldl (<>) mempty [V.error_message vErr, Just  ", field: ", V.error_field vErr]
              error_code = V.error_code vErr
            }
    }

invalidMandateMaxAmount :: Double -> ECErrorResponse
invalidMandateMaxAmount maxAm =
  ECErrorResponse
    { code = 400,
      response = A.encodePretty $ invalidMandateMaxAmountPayload maxAm
    }

invalidMandateMaxAmountPayload :: Double -> ECErrorPayload
invalidMandateMaxAmountPayload maxAm =
  ECErrorPayload
    { status = "ERROR",
      status_id = Just (-1),
      error_message = Just $ "maxAmount is mandatory and should be greater than Rs 0.00 and less than Rs " <> (pack . show $ maxAm),
      error_code = Just "INVALID_MANDATE_MAX_AMOUNT"
    }

dbError :: Text -> ErrorResponse
dbError x =
  ErrorResponse
    { code = 400,
      response =
        ErrorPayload
          { errorMessage = "Bad request.",
            userMessage = "Cannot find " <> x <> " in database.",
            error = True
          }
    }

dbsendError :: Text -> ErrorResponse
dbsendError x =
  ErrorResponse
    { code = 400,
      response =
        ErrorPayload
          { errorMessage = "Bad request.",
            userMessage = "Cannot create " <> x <> ".",
            error = True
          }
    }

dbUpdateError :: Text -> ErrorResponse
dbUpdateError x =
  ErrorResponse
    { code = 400,
      response =
        ErrorPayload
          { errorMessage = "Bad request.",
            userMessage = "Cannot update " <> x <> " in database.",
            error = True
          }
    }

urlError :: Text -> ErrorResponse
urlError x =
  ErrorResponse
    { code = 400,
      response =
        ErrorPayload
          { errorMessage = "Bad request.",
            userMessage = "Cannot find " <> x <> " in URL.",
            error = True
          }
    }

eulerAccessDenied :: Text -> ErrorResponse
eulerAccessDenied message =
  ErrorResponse
    { code = 403,
      response =
        ErrorPayload
          { errorMessage = "Forbidden." <> message,
            userMessage = "Access Denied. Unable to proceed.",
            error = True
          }
    }

throwECExceptionInvalid :: Text -> ECErrorResponse
throwECExceptionInvalid message =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Nothing,
              error_message = Just message,
              error_code = Just "invalid_request"
            }
    }

throwCustomException :: Text -> Text -> ECErrorResponse
throwCustomException errCode message =
  ECErrorResponse
    { code = 400,
      response =
        A.encodePretty $
          ECErrorPayload
            { status = "ERROR",
              status_id = Just (-1),
              error_message = Just message,
              error_code = Just errCode
            }
    }
