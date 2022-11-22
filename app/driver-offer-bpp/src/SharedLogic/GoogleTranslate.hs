module SharedLogic.GoogleTranslate
  ( translate,
  )
where

import qualified Beckn.External.GoogleTranslate.Client as ClientGoogleTranslate
import Beckn.External.GoogleTranslate.Types
import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Utils.Common (MonadFlow)
import EulerHS.Prelude
import Servant (ToHttpApiData (toUrlPiece))
import qualified Tools.Maps as Maps

translate :: (MonadFlow m, GoogleTranslate.HasGoogleTranslate m r, CoreMetrics m) => Maps.Language -> Maps.Language -> Text -> m GoogleTranslate.TranslateResp
translate source target q = do
  url <- asks (.googleTranslateUrl)
  apiKey <- asks (.googleTranslateKey)
  let srcEnc = toUrlPiece source
  let tgtEnc = toUrlPiece target
  if srcEnc == tgtEnc
    then
      return $
        TranslateResp
          { _data = Translation {translations = [TranslatedText {translatedText = q}]},
            _error = Nothing
          }
    else ClientGoogleTranslate.translate url apiKey srcEnc tgtEnc q
