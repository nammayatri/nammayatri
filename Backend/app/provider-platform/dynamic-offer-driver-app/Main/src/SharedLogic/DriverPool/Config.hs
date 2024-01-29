{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Client.Main as CM
import Data.Aeson as DA
-- import Domain.Types.Merchant.DriverPoolConfig as DPC

-- import Data.Aeson.Key
-- import qualified Data.Time.Clock as DTC
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text.Encoding as DTE
import Data.Aeson.Types as DAT
import Data.HashMap.Strict as HashMap
import Data.Text as Text
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Logging

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Variant.Variant ->
  Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId mbvt dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId)), (pack "tripDistance", DA.String (Text.pack (show dist)))] ++ (bool [] [(pack "variant", DA.String (Text.pack (show $ fromJust mbvt)))] (isJust mbvt)))
  logDebug $ "the context value is " <> show dpcCond
  contextValue <- liftIO $ CM.evalCtx "test" dpcCond
  case contextValue of
    Left err -> error $ (pack "error in fetching the context value ") <> (pack err)
    Right contextValue' -> do
      logDebug $ "the fetched context value is " <> show contextValue'
      --value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
      let valueHere = buildDpcType contextValue'
      logDebug $ "the build context value is1 " <> show valueHere
      return valueHere
  where
    buildDpcType cv =
      case (DAT.parse jsonToDriverPoolConfig cv) of
        Success dpc -> dpc
        Error err -> error $ (pack "error in parsing the context value ") <> (pack err)
