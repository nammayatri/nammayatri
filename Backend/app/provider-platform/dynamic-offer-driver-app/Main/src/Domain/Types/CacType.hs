module Domain.Types.CacType where

import qualified Data.Aeson as A
import Data.Text as Text
import Domain.Types.UtilsTH
import Kernel.Prelude
import Storage.Beam.DriverIntelligentPoolConfig as DIPC
import Storage.Beam.DriverPoolConfig as DPC
import qualified Storage.Beam.FarePolicy as FP
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as DEFB
import qualified Storage.Beam.FarePolicy.FarePolicyAmbulanceDetailsSlab as FPAD
import qualified Storage.Beam.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as FPICDPS
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as FPFB
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as FPPDPEKRS
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails as FPRD
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsDistanceBuffers as FPRDDB
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs as FPRDPS
import qualified Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as FPSS
import Storage.Beam.GoHomeConfig as GHC
import qualified Storage.Beam.MerchantServiceUsageConfig as MSUC
import Storage.Beam.TransporterConfig as MTC

checkParseCommon :: (String, A.Value) -> Bool
checkParseCommon (key, value) = do
  case Text.splitOn ":" (pack key) of
    [tableName, tableColumn] -> do
      case tableName of
        "driverPoolConfig" -> checkParse (Proxy @DPC.DriverPoolConfigT) tableColumn value
        "driverIntelligentPoolConfig" -> checkParse (Proxy @DIPC.DriverIntelligentPoolConfigT) tableColumn value
        "transporterConfig" -> checkParse (Proxy @MTC.TransporterConfigT) tableColumn value
        "goHomeConfig" -> checkParse (Proxy @GHC.GoHomeConfigT) tableColumn value
        "farePolicyProgressiveDetailsPerExtraKmRateSection" -> checkParse (Proxy @[FPPDPEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection]) tableColumn value
        "farePolicyRentalDetailsDistanceBuffers" -> checkParse (Proxy @[FPRDDB.FarePolicyRentalDetailsDistanceBuffers]) tableColumn value
        "farePolicyRentalDetailsPricingSlabs" -> checkParse (Proxy @[FPRDPS.FarePolicyRentalDetailsPricingSlabs]) tableColumn value
        "farePolicyInterCityDetailsPricingSlabs" -> checkParse (Proxy @[FPICDPS.FarePolicyInterCityDetailsPricingSlabs]) tableColumn value
        "farePolicySlabsDetailsSlab" -> checkParse (Proxy @[FPSS.FarePolicySlabsDetailsSlab]) tableColumn value
        "driverExtraFeeBounds" -> checkParse (Proxy @DEFB.DriverExtraFeeBoundsT) tableColumn value
        "farePolicyProgressiveDetails" -> checkParse (Proxy @FPFB.FarePolicyProgressiveDetailsT) tableColumn value
        "farePolicyRentalDetails" -> checkParse (Proxy @FPRD.FarePolicyRentalDetailsT) tableColumn value
        "farePolicy" -> checkParse (Proxy @FP.FarePolicyT) tableColumn value
        "farePolicyAmbulanceDetails" -> checkParse (Proxy @FPAD.FarePolicyAmbulanceDetailsSlabT) tableColumn value
        "merchantServiceUsageConfig" -> checkParse (Proxy @MSUC.MerchantServiceUsageConfigT) tableColumn value
        _ -> True
    _ -> True
