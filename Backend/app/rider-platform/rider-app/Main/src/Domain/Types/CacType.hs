module Domain.Types.CacType where

import qualified Data.Aeson as A
import Data.Text as Text
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.External.AadhaarVerification
import Kernel.External.Call.Types (CallService)
import Kernel.External.Maps.Types
import Kernel.External.Notification.Types (NotificationService)
import Kernel.External.SMS (SmsService)
import Kernel.External.Ticket.Types (IssueTicketService)
import Kernel.External.Whatsapp.Types (WhatsappService)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Beam.MerchantServiceUsageConfig as MSUC

checkParseCommon :: (String, A.Value) -> Bool
checkParseCommon (key, value) = do
  case Text.splitOn ":" (pack key) of
    [tableName, tableColumn] -> do
      case tableName of
        "merchantServiceUsageConfig" -> checkParse (Proxy @MSUC.MerchantServiceUsageConfig) tableColumn value
        _ -> False
    _ -> False

class CheckParse table where
  checkParse :: Proxy table -> Text -> A.Value -> Bool

checkField :: forall a. A.FromJSON a => Proxy a -> A.Value -> Bool
checkField _ value = case A.fromJSON @a value of
  A.Success _ -> True
  A.Error _ -> False

-- type family TransporterConfigT Symbol
--  where
--     TransporterConfigT "actualDistanceThreshold" = Meters

instance CheckParse MSUC.MerchantServiceUsageConfig where
  checkParse _ tableColumn value =
    case tableColumn of
      "merchantId" -> checkField (Proxy @(Id Merchant)) value
      "merchantOperatingCityId" -> checkField (Proxy @(Id MerchantOperatingCity)) value
      "initiateCall" -> checkField (Proxy @CallService) value
      "getDistances" -> checkField (Proxy @MapsService) value
      "getRoutes" -> checkField (Proxy @MapsService) value
      "snapToRoad" -> checkField (Proxy @MapsService) value
      "getPlaceName" -> checkField (Proxy @MapsService) value
      "getFrfsAutocompleteDistances" -> checkField (Proxy @MapsService) value
      "getPickupRoutes" -> checkField (Proxy @MapsService) value
      "getTripRoutes" -> checkField (Proxy @MapsService) value
      "getPlaceDetails" -> checkField (Proxy @MapsService) value
      "autoComplete" -> checkField (Proxy @MapsService) value
      "aadhaarVerificationService" -> checkField (Proxy @AadhaarVerificationService) value
      "getDistancesForCancelRide" -> checkField (Proxy @MapsService) value
      "notifyPerson" -> checkField (Proxy @NotificationService) value
      "useFraudDetection" -> checkField (Proxy @Bool) value
      "smsProvidersPriorityList" -> checkField (Proxy @SmsService) value
      "whatsappProvidersPriorityList" -> checkField (Proxy @WhatsappService) value
      "issueTicketService" -> checkField (Proxy @IssueTicketService) value
      "enableDashboardSms" -> checkField (Proxy @Bool) value
      "getExophone" -> checkField (Proxy @CallService) value
      "updatedAt" -> checkField (Proxy @UTCTime) value
      "createdAt" -> checkField (Proxy @UTCTime) value
      _ -> True
