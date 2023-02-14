 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Tools.Error (module Tools.Error) where

import EulerHS.Prelude
import Kernel.Types.Error as Tools.Error hiding (PersonError)
import Kernel.Types.Error.BaseError.HTTPError

data FarePolicyError
  = NoFarePolicy
  | NoPerExtraKmRate
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FarePolicyError

instance IsBaseError FarePolicyError where
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing

instance IsHTTPError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode NoPerExtraKmRate = "NO_PER_EXTRA_KM_RATE"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toHttpCode _ = E500

instance IsAPIError FarePolicyError

data RentalFarePolicyError
  = NoRentalFarePolicy
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''RentalFarePolicyError

instance IsBaseError RentalFarePolicyError where
  toMessage NoRentalFarePolicy = Just "No rental fare policy matches passed data."

instance IsHTTPError RentalFarePolicyError where
  toErrorCode NoRentalFarePolicy = "NO_RENTAL_FARE_POLICY"
  toHttpCode _ = E500

instance IsAPIError RentalFarePolicyError

data FPDiscountError
  = FPDiscountDoesNotExist
  | FPDiscountAlreadyEnabled
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FPDiscountError

instance IsBaseError FPDiscountError where
  toMessage = \case
    FPDiscountDoesNotExist -> Just "No discount matches passed data."
    FPDiscountAlreadyEnabled -> Just "Some discount is already enabled."

instance IsHTTPError FPDiscountError where
  toErrorCode = \case
    FPDiscountDoesNotExist -> "FARE_POLICY_DISCOUNT_DOES_NOT_EXIST"
    FPDiscountAlreadyEnabled -> "FARE_POLICY_DISCOUNT_ALREADY_ENABLED"
  toHttpCode = \case
    FPDiscountDoesNotExist -> E400
    FPDiscountAlreadyEnabled -> E400

instance IsAPIError FPDiscountError

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AllocationError

instance IsBaseError AllocationError

instance IsHTTPError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

instance IsAPIError AllocationError

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverInformationError

instance IsBaseError DriverInformationError

instance IsHTTPError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"

instance IsAPIError DriverInformationError

data ProductsError
  = ProductsNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ProductsError

instance IsBaseError ProductsError

instance IsHTTPError ProductsError where
  toErrorCode = \case
    ProductsNotFound -> "PRODUCTS_NOT_FOUND"
  toHttpCode = \case
    ProductsNotFound -> E500

instance IsAPIError ProductsError

newtype ShardMappingError = ShardMappingError Text
  deriving (Show, Typeable)

instance IsBaseError ShardMappingError where
  toMessage (ShardMappingError msg) = Just msg

instanceExceptionWithParent 'BaseException ''ShardMappingError

data DriverError
  = DriverAccountDisabled
  | DriverWithoutVehicle Text
  | DriverAccountBlocked
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverError

instance IsBaseError DriverError where
  toMessage DriverAccountDisabled = Just "Driver account has been disabled. He can't go online and receive ride offers in this state."
  toMessage (DriverWithoutVehicle personId) = Just $ "Driver with id = " <> personId <> " has no linked vehicle"
  toMessage DriverAccountBlocked = Just "Driver account has been blocked."

instance IsHTTPError DriverError where
  toErrorCode = \case
    DriverAccountDisabled -> "DRIVER_ACCOUNT_DISABLED"
    DriverWithoutVehicle _ -> "DRIVER_WITHOUT_VEHICLE"
    DriverAccountBlocked -> "DRIVER_ACCOUNT_BLOCKED"
  toHttpCode = \case
    DriverAccountDisabled -> E403
    DriverWithoutVehicle _ -> E500
    DriverAccountBlocked -> E403

instance IsAPIError DriverError

--
newtype OfferError
  = NotAllowedExtraFee Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''OfferError

instance IsBaseError OfferError where
  toMessage (NotAllowedExtraFee x) = Just $ "Not allowed extra fee: " <> x

instance IsHTTPError OfferError where
  toErrorCode = \case
    NotAllowedExtraFee {} -> "EXTRA_FEE_NOT_ALLOWED"
  toHttpCode = \case
    NotAllowedExtraFee {} -> E400

instance IsAPIError OfferError

newtype SearchRequestErrorARDU
  = SearchRequestNotRelevant Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SearchRequestErrorARDU

instance IsBaseError SearchRequestErrorARDU where
  toMessage (SearchRequestNotRelevant _) = Just "Search request no longer relevant"

instance IsHTTPError SearchRequestErrorARDU where
  toErrorCode = \case
    SearchRequestNotRelevant _ -> "SEARCH_REQUEST_NOT_RELEVANT"
  toHttpCode = \case
    SearchRequestNotRelevant _ -> E400

instance IsAPIError SearchRequestErrorARDU

--
data DriverQuoteError
  = FoundActiveQuotes
  | DriverOnRide
  | DriverQuoteExpired
  | NoSearchRequestForDriver
  | QuoteAlreadyRejected
  | UnexpectedResponseValue
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverQuoteError

instance IsBaseError DriverQuoteError where
  toMessage FoundActiveQuotes = Just "Failed to offer quote, there are other active quotes from this driver"
  toMessage DriverOnRide = Just "Unable to offer a quote while being on ride"
  toMessage DriverQuoteExpired = Just "Driver quote expired"
  toMessage NoSearchRequestForDriver = Just "No search request for this driver"
  toMessage QuoteAlreadyRejected = Just "Quote Aready Rejected"
  toMessage UnexpectedResponseValue = Just "The response type is unexpected"

instance IsHTTPError DriverQuoteError where
  toErrorCode = \case
    FoundActiveQuotes -> "FOUND_ACTIVE_QUOTES"
    DriverOnRide -> "DRIVER_ON_RIDE"
    DriverQuoteExpired -> "QUOTE_EXPIRED"
    NoSearchRequestForDriver -> "NO_SEARCH_REQUEST_FOR_DRIVER"
    QuoteAlreadyRejected -> "QUOTE_ALREADY_REJECTED"
    UnexpectedResponseValue -> "UNEXPECTED_RESPONSE_VALUE"
  toHttpCode = \case
    FoundActiveQuotes -> E400
    DriverOnRide -> E400
    DriverQuoteExpired -> E400
    NoSearchRequestForDriver -> E400
    QuoteAlreadyRejected -> E400
    UnexpectedResponseValue -> E400

instance IsAPIError DriverQuoteError

data FareParametersError
  = FareParametersNotFound Text
  | FareParametersDoNotExist Text
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FareParametersError

instance IsHTTPError FareParametersError where
  toErrorCode = \case
    FareParametersNotFound _ -> "FARE_PARAMETERS_NOT_FOUND"
    FareParametersDoNotExist _ -> "FARE_PARAMETERS_DO_NOT_EXIST"
  toHttpCode = \case
    FareParametersNotFound _ -> E500
    FareParametersDoNotExist _ -> E400

instance IsAPIError FareParametersError

instance IsBaseError FareParametersError where
  toMessage = \case
    FareParametersNotFound fareParamsId -> Just $ "FareParameters with fareParametersId \"" <> show fareParamsId <> "\" not found."
    FareParametersDoNotExist rideId -> Just $ "FareParameters for ride \"" <> show rideId <> "\" do not exist."
