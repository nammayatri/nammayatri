{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.GoogleTranslate
  ( translate,
    TranslateFlow,
    CacheTranslationConfig (..),
    HasCacheTranslationConfig,
  )
where

import EulerHS.Prelude
import Kernel.External.Encryption
import qualified Kernel.External.GoogleTranslate.Client as ClientGoogleTranslate
import Kernel.External.GoogleTranslate.Types
import qualified Kernel.External.GoogleTranslate.Types as GoogleTranslate
import Kernel.Prelude (HasField (..))
import Kernel.Storage.Hedis as Hedis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.App (HasFlowEnv)
import Kernel.Types.Field
import Kernel.Utils.Common (BaseUrl, MonadFlow, Seconds, logDebug)
import Kernel.Utils.Dhall
import Servant
import qualified Tools.Maps as Maps

newtype CacheTranslationConfig = CacheTranslationConfig
  { expTranslationTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheTranslationConfig r = HasField "cacheTranslationConfig" r CacheTranslationConfig

type TranslateFlow m r =
  ( HasCacheTranslationConfig r,
    HedisFlow m r,
    EncFlow m r,
    MonadFlow m,
    GoogleTranslate.HasGoogleTranslate m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  )

translate :: TranslateFlow m r => Text -> Maps.Language -> Maps.Language -> Text -> m GoogleTranslate.TranslateResp
translate someId source target q = do
  url <- asks (.googleTranslateUrl)
  apiKey <- asks (.googleTranslateKey)
  let tgtEnc = toUrlPiece target
  hashMessage <- getHash q
  if source == target
    then
      return $
        TranslateResp
          { _data = Translation {translations = [TranslatedText {translatedText = q}]},
            _error = Nothing
          }
    else findByTranslationKey hashMessage tgtEnc url apiKey
  where
    makeTranslationKey :: Text -> Text
    makeTranslationKey hashMessage = "Translation:" <> hashMessage

    cacheTranslation :: TranslateFlow m r => Text -> Text -> Translations -> m ()
    cacheTranslation translationKey tgtEnc translation = do
      expTime <- fromIntegral <$> asks (.cacheTranslationConfig.expTranslationTime)
      hSetExp translationKey tgtEnc translation expTime

    findByTranslationKey ::
      TranslateFlow m r =>
      Text ->
      Text ->
      BaseUrl ->
      Text ->
      m GoogleTranslate.TranslateResp
    findByTranslationKey hashMessage tgtEnc url apiKey = do
      let translationKey = makeTranslationKey hashMessage
      Hedis.hGet translationKey tgtEnc >>= \case
        Just a -> do
          logDebug "Translation got from Redis"
          return $
            TranslateResp
              { _data = a,
                _error = Nothing
              }
        Nothing -> do
          response <- ClientGoogleTranslate.translate url apiKey (toUrlPiece source) (toUrlPiece target) q someId
          cacheTranslation translationKey tgtEnc (_data response)
          return response
