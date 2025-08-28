{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Registry.Beckn.Nammayatri.Types
  ( module Reexport,
    module Registry.Beckn.Nammayatri.Types,
  )
where

import "mock-registry" App.Routes as Reexport
import "mock-registry" Domain.Subscriber as Reexport
import "mock-registry" Domain.Types.UpdateCities as Reexport
import EulerHS.Types (EulerClient, client)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse (..))
import qualified Kernel.Types.Beckn.City as Beckn
import Kernel.Utils.Dhall

data UpdData = UpdData
  { apiKey :: Text,
    appendCities :: Maybe [Beckn.City],
    replaceCities :: Maybe [Beckn.City]
  }
  deriving (Show, Generic)

updateCitiesClient :: Maybe Text -> UpdateCitiesReq -> EulerClient UpdateCitiesRes
updateCitiesClient = client updateCitiesAPI

createAPI :: Proxy CreateAPI
createAPI = Proxy

createSubscriberClient :: Subscriber -> EulerClient AckResponse
createSubscriberClient = client createAPI

data RegistryConfig = RegistryConfig
  { url :: BaseUrl,
    apiKey :: Text
  }
  deriving (Show, Generic, FromDhall)
