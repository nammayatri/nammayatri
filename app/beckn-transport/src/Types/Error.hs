{-# LANGUAGE DeriveAnyClass #-}

module Types.Error (module Types.Error) where

import Beckn.TypeClass.IsAPIError
import Beckn.Types.Error as Types.Error
import EulerHS.Prelude

data FarePolicyError
  = NoFarePolicy
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError FarePolicyError where
  toAPIError NoFarePolicy = APIError "NO_FARE_POLICY" "No fare policy matches passed data."
  toAPIError CantCalculateDistance = APIError "CANT_CALCULATE_DISTANCE" "Could not calculate distance."
  toStatusCode NoFarePolicy = E400
  toStatusCode CantCalculateDistance = E500

data AllocationError
  = EmptyDriverPool
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError AllocationError where
  toAPIError EmptyDriverPool = APIError "EMPTY_DRIVER_POOL" "No drivers nearby."
  toStatusCode EmptyDriverPool = E500

data DriverInformationError
  = DriverInfoNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError DriverInformationError where
  toAPIError DriverInfoNotFound = APIError "DRIVER_INFORMATON_NOT_FOUND" "Driver information not found."
  toStatusCode DriverInfoNotFound = E500

data HealthCheckError
  = ServiceUnavailable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError HealthCheckError where
  toAPIError ServiceUnavailable = APIError "SERVICE_UNAVAILABLE" "Service is down."
  toStatusCode ServiceUnavailable = E503
