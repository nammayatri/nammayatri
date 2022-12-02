module SharedLogic.GoogleTranslate
  ( translate,
    TranslateFlow,
    CacheTranslationConfig (..),
    HasCacheTranslationConfig,
  )
where

import Beckn.External.Encryption
import qualified Beckn.External.GoogleTranslate.Client as ClientGoogleTranslate
import Beckn.External.GoogleTranslate.Types
import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Prelude (HasField (..))
import Beckn.Storage.Hedis as Hedis
import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Utils.Common (BaseUrl, MonadFlow, Seconds, logDebug)
import Beckn.Utils.Dhall
import EulerHS.Prelude
import Servant
import qualified Tools.Maps as Maps

newtype CacheTranslationConfig = CacheTranslationConfig
  { expTranslationTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheTranslationConfig r = HasField "cacheTranslationConfig" r CacheTranslationConfig

type TranslateFlow m r = (HasCacheTranslationConfig r, HedisFlow m r, EncFlow m r, MonadFlow m, GoogleTranslate.HasGoogleTranslate m r, CoreMetrics m)

translate :: TranslateFlow m r => Maps.Language -> Maps.Language -> Text -> m GoogleTranslate.TranslateResp
translate source target q = do
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
          response <- ClientGoogleTranslate.translate url apiKey (toUrlPiece source) (toUrlPiece target) q
          cacheTranslation translationKey tgtEnc (_data response)
          return response
