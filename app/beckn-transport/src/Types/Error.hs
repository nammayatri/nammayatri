{-# LANGUAGE DeriveAnyClass #-}

module Types.Error (module Types.Error) where

import Beckn.TypeClass.IsDomainError
import Beckn.Types.Error as Types.Error
import EulerHS.Prelude

data FarePolicyError
  = FarePolicyDoesNotExist
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError FarePolicyError where
  toError FarePolicyDoesNotExist = APIError "FARE_POLICY_DOES_NOT_EXIST" "No fare policy matches passed data."
  toStatusCode FarePolicyDoesNotExist = E400

data AllocationError
  = EmptyDriverPool
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError AllocationError where
  toError EmptyDriverPool = APIError "EMPTY_DRIVER_POOL" "No drivers nearby."
  toStatusCode EmptyDriverPool = E400

data DriverInformationError
  = DriverInfoNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError DriverInformationError where
  toError DriverInfoNotFound = APIError "DRIVER_INFORMATON_NOT_FOUND" "Driver information not found."
  toStatusCode DriverInfoNotFound = E500

data HealthCheckError
  = ServiceUnavailable
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsDomainError HealthCheckError where
  toError ServiceUnavailable = APIError "SERVICE_UNAVAILABLE" "Service is down."
  toStatusCode ServiceUnavailable = E503
