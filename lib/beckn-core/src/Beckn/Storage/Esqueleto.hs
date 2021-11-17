module Beckn.Storage.Esqueleto
  ( module Types,
    module Functions,
    module Queries,
    module Logger,
    module Config,
    module SqlDB,
    defaultQQ,
  )
where

import Beckn.Storage.Esqueleto.Config as Config (EsqDBFlow)
import Beckn.Storage.Esqueleto.Functions as Functions
import Beckn.Storage.Esqueleto.Logger as Logger (LoggerIO)
import Beckn.Storage.Esqueleto.Queries as Queries
import Beckn.Storage.Esqueleto.SqlDB as SqlDB
import Beckn.Storage.Esqueleto.Types as Types
import Beckn.Utils.Text
import Database.Persist.Quasi.Internal
import Database.Persist.TH
import EulerHS.Prelude hiding (Key)
import Language.Haskell.TH.Quote

defaultQQ :: QuasiQuoter
defaultQQ =
  persistWith $
    upperCaseSettings
      { psToDBName = camelCaseToSnakeCase
      }
