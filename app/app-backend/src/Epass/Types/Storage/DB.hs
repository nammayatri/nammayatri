{-# LANGUAGE DeriveAnyClass #-}

module Epass.Types.Storage.DB where

import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
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
import qualified Epass.Types.Storage.Tag as Tag
import qualified Epass.Types.Storage.User as User
import EulerHS.Prelude hiding (id)
import Storage.DB.Config (dbSchema)

data EpassDb f = EpassDb
  { _user :: f (B.TableEntity User.UserT),
    _quota :: f (B.TableEntity Quota.QuotaT),
    _location :: f (B.TableEntity Location.LocationT),
    _Blacklist :: f (B.TableEntity Blacklist.BlacklistT),
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
      { _user = setSchema <> User.fieldEMod,
        _quota = setSchema <> Quota.fieldEMod,
        _location = setSchema <> Location.fieldEMod,
        _Blacklist = setSchema <> Blacklist.fieldEMod,
        _allocatedQuota = setSchema <> AllocatedQuota.fieldEMod,
        _customer = setSchema <> Customer.fieldEMod,
        _CustomerDetail = setSchema <> CustomerDetail.fieldEMod,
        _organization = setSchema <> Organization.fieldEMod,
        _pass = setSchema <> Pass.fieldEMod,
        _passApplication = setSchema <> PassApplication.fieldEMod,
        _document = setSchema <> Document.fieldEMod,
        _entityTag = setSchema <> EntityTag.fieldEMod,
        _entityDocument = setSchema <> EntityDocument.fieldEMod,
        _comment = setSchema <> Comment.fieldEMod,
        _tag = setSchema <> Tag.fieldEMod
      }
  where
    setSchema = setEntitySchema (Just dbSchema)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
