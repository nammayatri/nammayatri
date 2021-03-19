{-# LANGUAGE DeriveAnyClass #-}

module Types.Error where

import Beckn.TypeClass.IsError
import Beckn.Types.Error
import EulerHS.Prelude

data FarePolicyError
  = FarePolicyNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError FarePolicyError APIError where
  toError FarePolicyNotFound = APIError "FARE_POLICY_NOT_FOUND" $ Just "Fare policy not found."

data AllocationError
  = EmptyDriverPool
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError AllocationError APIError where
  toError EmptyDriverPool = APIError "EMPTY_DRIVER_POOL" $ Just "No drivers nearby."

data DriverInformationError
  = DriverInfoNotFound
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsError DriverInformationError APIError where
  toError DriverInfoNotFound = APIError "DRIVER_INFORMATON_NOT_FOUND" $ Just "Driver information not found."
