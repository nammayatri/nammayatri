{-# LANGUAGE DeriveAnyClass #-}

module Epass.Types.Storage.DB where

import qualified Database.Beam as B
import qualified Epass.Types.Storage.AllocatedQuota as AllocatedQuota
import qualified Epass.Types.Storage.Blacklist as Blacklist
import qualified Epass.Types.Storage.Comment as Comment
import qualified Epass.Types.Storage.Customer as Customer
import qualified Epass.Types.Storage.CustomerDetail as CustomerDetail
import qualified Epass.Types.Storage.Document as Document
import qualified Epass.Types.Storage.EntityDocument as EntityDocument
import qualified Epass.Types.Storage.EntityTag as EntityTag
import qualified Epass.Types.Storage.Location as Location
import qualified Epass.Types.Storage.Organization as Organization
import qualified Epass.Types.Storage.Pass as Pass
import qualified Epass.Types.Storage.PassApplication as PassApplication
import qualified Epass.Types.Storage.Quota as Quota
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Epass.Types.Storage.Tag as Tag
import qualified Epass.Types.Storage.User as User
import EulerHS.Prelude hiding (id)

data EpassDb f = EpassDb
  { _user :: f (B.TableEntity User.UserT),
    _quota :: f (B.TableEntity Quota.QuotaT),
    _location :: f (B.TableEntity Location.LocationT),
    _Blacklist :: f (B.TableEntity Blacklist.BlacklistT),
    _registrationToken :: f (B.TableEntity RegistrationToken.RegistrationTokenT),
    _allocatedQuota :: f (B.TableEntity AllocatedQuota.AllocatedQuotaT),
    _customer :: f (B.TableEntity Customer.CustomerT),
    _CustomerDetail :: f (B.TableEntity CustomerDetail.CustomerDetailT),
    _organization :: f (B.TableEntity Organization.OrganizationT),
    _pass :: f (B.TableEntity Pass.PassT),
    _passApplication :: f (B.TableEntity PassApplication.PassApplicationT),
    _document :: f (B.TableEntity Document.DocumentT),
    _entityTag :: f (B.TableEntity EntityTag.EntityTagT),
    _entityDocument :: f (B.TableEntity EntityDocument.EntityDocumentT),
    _comment :: f (B.TableEntity Comment.CommentT),
    _tag :: f (B.TableEntity Tag.TagT)
  }
  deriving (Generic, B.Database be)

becknDb :: B.DatabaseSettings be EpassDb
becknDb =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _user = User.fieldEMod,
        _quota = Quota.fieldEMod,
        _location = Location.fieldEMod,
        _Blacklist = Blacklist.fieldEMod,
        _registrationToken = RegistrationToken.fieldEMod,
        _allocatedQuota = AllocatedQuota.fieldEMod,
        _customer = Customer.fieldEMod,
        _CustomerDetail = CustomerDetail.fieldEMod,
        _organization = Organization.fieldEMod,
        _pass = Pass.fieldEMod,
        _passApplication = PassApplication.fieldEMod,
        _document = Document.fieldEMod,
        _entityTag = EntityTag.fieldEMod,
        _entityDocument = EntityDocument.fieldEMod,
        _comment = Comment.fieldEMod,
        _tag = Tag.fieldEMod
      }
