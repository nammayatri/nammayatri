{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.OcrProviderResponse
  ( OcrProviderResponseEvent (..),
    pushOcrProviderResponse,
  )
where

import Kernel.Beam.Lib.Utils (pushToKafka)
import Kernel.Prelude
import Kernel.Utils.Common

ocrProviderResponseTopic :: Text
ocrProviderResponseTopic = "driver-ocr-provider-response"

data OcrProviderResponseEvent = OcrProviderResponseEvent
  { id :: Text,
    merchant_id :: Text,
    merchant_operating_city_id :: Text,
    provider_type :: Maybe Text,
    image_type :: Text,
    response :: Text,
    created_at :: UTCTime,
    updated_at :: UTCTime
  }
  deriving (Generic, Show, ToJSON)

pushOcrProviderResponse ::
  MonadFlow m =>
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Text ->
  Text ->
  UTCTime ->
  m ()
pushOcrProviderResponse id' merchantId merchantOpCityId mbProviderType imageType responseText now =
  pushToKafka
    OcrProviderResponseEvent
      { id = id',
        merchant_id = merchantId,
        merchant_operating_city_id = merchantOpCityId,
        provider_type = mbProviderType,
        image_type = imageType,
        response = responseText,
        created_at = now,
        updated_at = now
      }
    ocrProviderResponseTopic
    id'
