module Beckn.Utils.Common
  ( module Beckn.Utils.Common,
    module Common,
  )
where

import Beckn.Types.App
import Beckn.Types.Common as Common
import Beckn.Types.Core.Ack as Common
import Beckn.Types.Core.Error
import Beckn.Types.Error.APIError as Common
import Beckn.Types.Field as Common
import Beckn.Utils.Context as Common
import Beckn.Utils.Error as Common
import Beckn.Utils.Flow as Common
import Beckn.Utils.Logging as Common
import Beckn.Utils.Servant.Client as Common
import Beckn.Utils.Text as Common
import Beckn.Utils.Time as Common
import EulerHS.Prelude hiding (id)

-- TODO: remove
checkAckResponseError :: (MonadThrow m, Common.Log m, Common.IsAPIException e) => (Error -> e) -> Common.AckResponse -> m ()
checkAckResponseError _ Common.Ack = pure ()

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

identity :: p -> p
identity a = a
