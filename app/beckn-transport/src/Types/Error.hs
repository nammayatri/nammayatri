{-# LANGUAGE DeriveAnyClass #-}

module Types.Error where

import Beckn.TypeClass.IsError
import Beckn.Types.Error
import EulerHS.Prelude

data FarePolicyError
  = FarePolicyNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError FarePolicyError APIError where
  toError FarePolicyNotFound = apiErrorWithMsg "FARE_POLICY_NOT_FOUND" "Fare policy not found."

data AllocationError
  = EmptyDriverPool
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AllocationError APIError where
  toError EmptyDriverPool = apiErrorWithMsg "EMPTY_DRIVER_POOL" "No drivers nearby."

data DriverInformationError
  = DriverInfoNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DriverInformationError APIError where
  toError DriverInfoNotFound = apiErrorWithMsg "DRIVER_INFORMATON_NOT_FOUND" "Driver information not found."
