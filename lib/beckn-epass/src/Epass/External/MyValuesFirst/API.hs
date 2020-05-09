module Epass.External.MyValuesFirst.API where

import           EulerHS.Prelude

import           Servant
import           Servant.API.ContentTypes
import           Servant.Client
import qualified EulerHS.Types as ET
import           Epass.Types.App
    ( MandatoryQueryParam
    )
import           Epass.External.MyValuesFirst.Types

type ServiceAPI =
  "smpp"
    :> "sendsms"
    :> MandatoryQueryParam "username" Text
    :> MandatoryQueryParam "password" Text
    :> MandatoryQueryParam "from" Text
    :> MandatoryQueryParam "to" Text
    :> MandatoryQueryParam "text" Text
    :> Post '[PlainText] NoContent

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

submitSms SubmitSms{..} =
  void $ ET.client serviceAPI _username _password _from _to _text
