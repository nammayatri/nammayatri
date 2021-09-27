module Beckn.Utils.Common
  ( module Beckn.Utils.Common,
    module Common,
    callBecknAPI,
  )
where

import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Common
import Beckn.Types.Error.BaseError.HTTPError as Common
import Beckn.Types.Field as Common
import Beckn.Types.Id (ShortId (ShortId))
import Beckn.Utils.Context as Common
import Beckn.Utils.Error as Common
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI)
import Beckn.Utils.Error.DB as Common
import Beckn.Utils.Logging as Common
import Beckn.Utils.Servant.Client as Common
import Beckn.Utils.Text as Common
import Beckn.Utils.Time as Common
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified Test.RandomStrings as RS

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

identity :: p -> p
identity a = a

maskText :: Text -> Text
maskText text =
  if length text > 6
    then T.take 3 text <> "..." <> T.takeEnd 3 text
    else "..."

generateShortId :: MonadFlow m => m (ShortId a)
generateShortId = ShortId . T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 10)
