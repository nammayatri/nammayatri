{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Registry.Beckn.Interface.Types where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Beckn
import Kernel.Types.Common
import qualified Kernel.Types.Registry as Beckn
import Kernel.Utils.Common
import Kernel.Utils.JSON (removeNullFields)
import qualified Registry.Beckn.Nammayatri.Types as NyT

data RegistryReq a = RegistryReq
  { uniqueKeyId :: Text,
    subscriberId :: Text,
    subscriberType :: Beckn.SubscriberType,
    domain :: Beckn.Domain,
    registryUrl :: BaseUrl,
    _data :: a
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON a => ToJSON (RegistryReq a) where
  toJSON = genericToJSON removeNullFields

deriving instance ToSchema a => ToSchema (RegistryReq a)

type UpdateSubscriberReq = RegistryReq UpdateSubscriber

newtype UpdateSubscriber
  = UpdSubNY NyT.UpdData

newtype UpdateSubscriberResp
  = UpdSubResNY NyT.UpdateCitiesRes

buildAddCityNyReq ::
  HasFlowEnv m r '["nammayatriRegistryConfig" ::: NyT.RegistryConfig] =>
  NonEmpty Beckn.City ->
  Text ->
  Text ->
  Beckn.SubscriberType ->
  Beckn.Domain ->
  m UpdateSubscriberReq
buildAddCityNyReq newCities uniqueKeyId subscriberId subscriberType domain = do
  NyT.RegistryConfig {..} <- asks (.nammayatriRegistryConfig)
  pure $
    RegistryReq
      { _data =
          UpdSubNY $
            NyT.UpdData
              { apiKey,
                appendCities = Just $ NE.toList newCities,
                replaceCities = Nothing
              },
        registryUrl = url,
        ..
      }
