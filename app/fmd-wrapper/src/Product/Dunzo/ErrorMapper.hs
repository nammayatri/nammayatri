module Product.Dunzo.ErrorMapper where

import Beckn.Types.Core.Error as Beckn
import EulerHS.Prelude
import External.Dunzo.Types as Dunzo

mapError :: Dunzo.Error -> Beckn.Error
mapError Dunzo.Error {..} =
  Beckn.Error
    { _type = "DOMAIN-ERROR",
      _code = becknErrCode,
      _path = Nothing,
      _message = Just message
    }
  where
    becknErrCode = case code of
      "unauthorized" -> "CORE001"
      "rate_limit_exceeded" -> "CORE002"
      "validation_failed" -> "CORE003"
      "bad_request" -> "CORE003"
      "unserviceable_location_error" -> "FMD001"
      "stock_out_error" -> "FMD009"
      "internal_server_error" -> "CORE002"
      "duplicate_request" -> "CORE003"
      "rain_error" -> "FMD009"
      "different_city_error" -> "FMD001"
      "near_by_location_error" -> "FMD001"
      "service_unavailable" -> "CORE002"
      "default" -> "CORE003"
      _ -> "CORE003"
