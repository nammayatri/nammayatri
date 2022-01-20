module Utils.Callback where

import App.Types
import Beckn.Utils.Callback
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Control.Lens.Operators ((?~))
import EulerHS.Prelude hiding (drop)
import qualified EulerHS.Types as ET
import Utils.Common

withCallback ::
  WithBecknCallbackMig api callback_success Flow
withCallback = withCallback' identity

withCallback' ::
  (Flow () -> Flow ()) ->
  WithBecknCallbackMig api callback_success Flow
withCallback' doWithCallback action api context cbUrl f = do
  bppUri <- asks (.nwAddress)
  selfId <- asks (.selfId)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ selfId
  withBecknCallbackMig doWithCallback authKey action api context' cbUrl f

authKey :: Maybe ET.ManagerSelector
authKey = Just HttpSig.signatureAuthManagerKey
