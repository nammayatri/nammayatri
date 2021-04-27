module Beckn.Types.Error.HttpCode where

import EulerHS.Prelude
import Servant.Server.Internal

data HttpCode
  = E400
  | E401
  | E402
  | E403
  | E404
  | E500
  | E501
  | E503
  deriving (Show)

toServerError :: HttpCode -> ServerError
toServerError = \case
  E400 -> err400
  E401 -> err401
  E402 -> err402
  E403 -> err403
  E404 -> err404
  E500 -> err500
  E501 -> err501
  E503 -> err503

isInternalError :: HttpCode -> Bool
isInternalError = \case
  E400 -> False
  E401 -> False
  E402 -> False
  E403 -> False
  E404 -> False
  E500 -> True
  E501 -> True
  E503 -> True
