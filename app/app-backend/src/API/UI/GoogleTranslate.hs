module API.UI.GoogleTranslate
  ( API,
    handler,
  )
where

import qualified Beckn.External.GoogleTranslate.Client as ClientGoogleTranslate
import Beckn.External.GoogleTranslate.Types
import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI, withPersonIdLogTag)
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Servant
import Tools.Auth

type API =
  "language"
    :> ( "translate"
           :> TokenAuth
           :> MandatoryQueryParam "source" Text
           :> MandatoryQueryParam "target" Text
           :> MandatoryQueryParam "q" Text
           :> Get '[JSON] GoogleTranslate.TranslateResp
       )

handler :: FlowServer API
handler =
  translate

translate :: Id Person.Person -> Text -> Text -> Text -> FlowHandler GoogleTranslate.TranslateResp
translate personId source target q = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleTranslateUrl)
  apiKey <- asks (.googleTranslateKey)
  if source == target
    then
      return $
        TranslateResp
          { _data = Translation {translations = [TranslatedText {translatedText = q}]},
            _error = Nothing
          }
    else ClientGoogleTranslate.translate url apiKey (toUrlPiece source) (toUrlPiece target) q
