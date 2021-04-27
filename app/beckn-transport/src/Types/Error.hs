{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.APIError
import EulerHS.Prelude

data FarePolicyError
  = NoFarePolicy
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instance IsAPIError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing
  toHttpCode NoFarePolicy = E400
  toHttpCode CantCalculateDistance = E500

instanceExceptionWithParent 'APIException ''FarePolicyError

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AllocationError

instance IsAPIError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''DriverInformationError

instance IsAPIError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"
