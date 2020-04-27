module Beckn.Types.Storage.DB where

import qualified Beckn.Types.Storage.Customer as Customer
import qualified Database.Beam                as B
import           EulerHS.Prelude

data BecknDB f =
  BecknDB
  { _customer :: f (B.TableEntity Customer.CustomerT)
  }
  deriving (Generic, B.Database be)

becknDB :: B.DatabaseSettings be BecknDB
becknDB =
  B.defaultDbSettings `B.withDbModification`
  B.dbModification
    { _customer = Customer.fieldEMod
    }
