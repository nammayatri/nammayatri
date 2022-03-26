module API.Error where

import Beckn.Prelude
import Data.Aeson (KeyValue ((.=)), Value (Null), object)
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API

differentCityError :: API.Error
differentCityError =
  API.Error
    { code = "different_city_error",
      message = "Apologies, our services are limited to serviceable areas with in the city only.",
      details = Null
    }

unauthorized :: API.Error
unauthorized =
  API.Error
    { code = "unauthorized",
      message = "Invalid username or password.",
      details = Null
    }

validationFailed :: Text -> API.Error
validationFailed param =
  API.Error
    { code = "validation_failed",
      message = "The parameters of your request were invalid.",
      details = object ["param" .= param]
    }

duplicateRequest :: API.TaskId -> API.Error
duplicateRequest taskId =
  API.Error
    { code = "duplicate_request",
      message = "Request with same request id has already been processed.",
      details = object ["task_id" .= taskId]
    }

badRequest :: API.Error
badRequest =
  API.Error
    { code = "bad_request",
      message = "task_data not found",
      details = Null
    }

internalError :: Text -> API.Error
internalError message =
  API.Error
    { code = "internal_error",
      message = message,
      details = Null
    }
