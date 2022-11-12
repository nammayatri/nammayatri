module API.UI.GoogleTranslate
  ( API,
    handler,
  )
where

import qualified Beckn.External.GoogleTranslate.Client as ClientGoogleTranslate
import qualified Beckn.External.GoogleTranslate.Types as GoogleTranslate
import Beckn.Types.App
import Beckn.Utils.Common (withFlowHandlerAPI, withPersonIdLogTag)
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Servant
import Tools.Auth
import qualified Domain.Types.Person as Person
import Beckn.Types.Id

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
  ClientGoogleTranslate.translate url apiKey source target q
