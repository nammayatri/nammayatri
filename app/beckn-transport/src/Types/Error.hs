{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.BaseError.APIError
import Beckn.Types.Error.BaseError.APIError.DomainError
import EulerHS.Prelude

data FarePolicyError
  = NoFarePolicy
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instanceExceptionWithParent 'DomainException ''FarePolicyError

instance IsBaseError FarePolicyError where
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing

instance IsAPIError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toHttpCode NoFarePolicy = E400
  toHttpCode CantCalculateDistance = E500

instance IsDomainError FarePolicyError

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show)

instanceExceptionWithParent 'DomainException ''AllocationError

instance IsBaseError AllocationError

instance IsAPIError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

instance IsDomainError AllocationError

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'DomainException ''DriverInformationError

instance IsBaseError DriverInformationError

instance IsAPIError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"

instance IsDomainError DriverInformationError

data ProductsError
  = ProductsNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'DomainException ''ProductsError

instance IsBaseError ProductsError

instance IsAPIError ProductsError where
  toErrorCode = \case
    ProductsNotFound -> "PRODUCTS_NOT_FOUND"
  toHttpCode = \case
    ProductsNotFound -> E500

instance IsDomainError ProductsError

newtype ShardMappingError = ShardMappingError Text
  deriving (Show, Typeable)

instance IsBaseError ShardMappingError where
  toMessage (ShardMappingError msg) = Just msg

instanceExceptionWithParent 'BaseException ''ShardMappingError
