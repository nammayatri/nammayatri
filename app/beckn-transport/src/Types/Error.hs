{-# LANGUAGE DeriveAnyClass #-}

module Types.Error (module Types.Error) where

import Beckn.TypeClass.IsAPIError
import Beckn.Types.Error as Types.Error
import EulerHS.Prelude

data FarePolicyError
  = FarePolicyDoesNotExist
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError FarePolicyError where
  toAPIError FarePolicyDoesNotExist = APIError "FARE_POLICY_DOES_NOT_EXIST" "No fare policy matches passed data."
  toStatusCode FarePolicyDoesNotExist = E400

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
