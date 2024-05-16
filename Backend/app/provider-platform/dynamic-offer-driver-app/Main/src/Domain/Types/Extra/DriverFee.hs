{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Extra.DriverFee where

import Data.Text (Text)
import Kernel.Prelude

-- Extra code goes here --

paymentProcessingLockKey :: Text -> Text
paymentProcessingLockKey driverId = "Payment:Processing:DriverId" <> driverId

mandateProcessingLockKey :: Text -> Text
mandateProcessingLockKey driverId = "Mandate:Processing:DriverId" <> driverId

billNumberGenerationLockKey :: Text -> Text
billNumberGenerationLockKey merchantOpCityId = "DriverFee:BillNumber:Processing:MerchantOpCityId" <> merchantOpCityId --- make lock on merchant Id
