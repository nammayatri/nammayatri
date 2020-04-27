{-# LANGUAGE DeriveAnyClass #-}

module Beckn.Types.Storage.DB where

import           EulerHS.Prelude                       hiding (id)

import qualified Beckn.Types.Storage.AllocatedQuota    as AllocatedQuota
import qualified Beckn.Types.Storage.Customer          as Customer
import qualified Beckn.Types.Storage.CustomerDetails   as CustomerDetails
import qualified Beckn.Types.Storage.Location          as Location
import qualified Beckn.Types.Storage.LocationBlacklist as LocationBlacklist
import qualified Beckn.Types.Storage.Organization      as Organization
import qualified Beckn.Types.Storage.Pass              as Pass
import qualified Beckn.Types.Storage.Quota             as Quota
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Beckn.Types.Storage.User              as User

import qualified Database.Beam                         as B



data BecknDb f =
  BecknDb
    { _user   :: f (B.TableEntity User.UserT)
     , _quota :: f (B.TableEntity Quota.QuotaT)
     , _location  :: f (B.TableEntity Location.LocationT)
     , _locationBlacklist  :: f (B.TableEntity LocationBlacklist.LocationBlacklistT)
     , _registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT)
     , _allocatedQuota :: f (B.TableEntity AllocatedQuota.AllocatedQuotaT)
     , _customer :: f (B.TableEntity Customer.CustomerT)
     , _customerDetails :: f (B.TableEntity CustomerDetails.CustomerDetailsT)
     , _organization :: f (B.TableEntity Organization.OrganizationT)
     , _pass :: f (B.TableEntity Pass.PassT)
     }
  deriving (Generic, B.Database be)

becknDb :: B.DatabaseSettings be BecknDb
becknDb =
  B.defaultDbSettings `B.withDbModification`
      B.dbModification
        { _user = User.fieldEMod
        , _quota = Quota.fieldEMod
        , _location = Location.fieldEMod
        , _locationBlacklist = LocationBlacklist.fieldEMod
        , _registrationToken = RegistrationToken.fieldEMod
        , _allocatedQuota = AllocatedQuota.fieldEMod
        , _customer = Customer.fieldEMod
        , _customerDetails = CustomerDetails.fieldEMod
        , _organization = Organization.fieldEMod
        , _pass = Pass.fieldEMod
        }
