{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.DriverFee where

import Kernel.Prelude

-- Extra code goes here --

paymentProcessingLockKey :: Text -> Text
paymentProcessingLockKey driverId = "Payment:Processing:DriverId" <> driverId

manualPaymentInProgressKey :: Text -> Text
manualPaymentInProgressKey driverFeeId = "Payment:Manual:InProgress:DriverFeeId:" <> driverFeeId

manualPaymentInProgressTtl :: Int
manualPaymentInProgressTtl = 1800

mandateProcessingLockKey :: Text -> Text
mandateProcessingLockKey driverId = "Mandate:Processing:DriverId" <> driverId

billNumberGenerationLockKey :: Text -> Text
billNumberGenerationLockKey billNumberKey = "DriverFee:BillNumber:Processing:" <> billNumberKey
