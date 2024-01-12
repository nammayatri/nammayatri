module Domain.Types.CacType where

import qualified Data.Aeson as A
import Data.Text as Text
import Domain.Types.UtilsTH
import Kernel.Prelude
import Storage.Beam.DriverPoolConfig as DPC
import qualified Storage.Beam.FarePolicy.DriverExtraFeeBounds as DEFB
import qualified Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as FPFB
import qualified Storage.Beam.FarePolicy.FarePolicyRentalDetails as FPRD
import qualified Storage.Beam.FarePolicyCACTypes as FPC
import Storage.Beam.GoHomeConfig as GHC
import Storage.Beam.Merchant.DriverIntelligentPoolConfig as DIPC
import Storage.Beam.Merchant.TransporterConfig as MTC

checkParseCommon :: (String, A.Value) -> Bool
checkParseCommon (key, value) = do
  case Text.splitOn ":" (pack key) of
    [tableName, tableColumn] -> do
      case tableName of
        "driverPoolConfig" -> checkParse (Proxy @DPC.DriverPoolConfigT) tableColumn value
        "driverIntelligentPoolConfig" -> checkParse (Proxy @DIPC.DriverIntelligentPoolConfigT) tableColumn value
        "transporterConfig" -> checkParse (Proxy @MTC.TransporterConfigT) tableColumn value
        "goHomeConfig" -> checkParse (Proxy @GHC.GoHomeConfigT) tableColumn value
        "farePolicyProgressiveDetailsPerExtraKmRateSection" -> checkParse (Proxy @[FPC.FarePolicyProgressiveDetailsPerExtraKmRateSection]) tableColumn value
        "farePolicyRentalDetailsDistanceBuffers" -> checkParse (Proxy @[FPC.FarePolicyRentalDetailsDistanceBuffers]) tableColumn value
        "farePolicySlabsDetailsSlab" -> checkParse (Proxy @[FPC.FarePolicySlabsDetailsSlab]) tableColumn value
        "driverExtraFeeBounds" -> checkParse (Proxy @DEFB.DriverExtraFeeBoundsT) tableColumn value
        "farePolicyProgressiveDetails" -> checkParse (Proxy @FPFB.FarePolicyProgressiveDetailsT) tableColumn value
        "farePolicyRentalDetails" -> checkParse (Proxy @FPRD.FarePolicyRentalDetailsT) tableColumn value
        _ -> True
    _ -> True
