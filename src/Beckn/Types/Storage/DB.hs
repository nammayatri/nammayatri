{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Storage.DB where

import           EulerHS.Prelude                       hiding (id)

import qualified Beckn.Types.Storage.Customer          as Customer
import qualified Beckn.Types.Storage.LocationBlacklist as LocationBlacklist
import qualified Beckn.Types.Storage.Quota             as Quota
import qualified Beckn.Types.Storage.User              as User

import qualified Database.Beam                         as B



data BecknDb f =
  BecknDb
    { _user   :: f (B.TableEntity User.UserT)
    , _quota :: f (B.TableEntity Quota.QuotaT)
    , _locationBlacklist  :: f (B.TableEntity LocationBlacklist.LocationBlacklistT)
    ,_customer :: f (B.TableEntity Customer.CustomerT)
    }
  deriving (Generic, B.Database be)

becknDb :: B.DatabaseSettings be BecknDb
becknDb =
  B.defaultDbSettings `B.withDbModification`
      B.dbModification
        { _user = User.fieldEMod
        , _quota = Quota.fieldEMod
        , _locationBlacklist = LocationBlacklist.fieldEMod
        , _customer = Customer.fieldEMod
        }
