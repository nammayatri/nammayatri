{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Epass.Types.App
  ( module Epass.Types.App,
    module Beckn.Types.App,
  )
where

import Beckn.Types.App
import Beckn.Utils.TH
import Data.Swagger
import Database.Beam.Backend.SQL
  ( FromBackendRow,
    HasSqlValueSyntax,
  )
import Database.Beam.MySQL (MySQL, MysqlValueSyntax)
import Database.Beam.MySQL.FromField
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Runtime as R
import Servant
import Servant.Swagger

newtype CustomerId = CustomerId
  { _getCustomerId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''CustomerId

-- newtype OrganizationId = OrganizationId
--   { _getOrganizationId :: Text
--   }
--   deriving (Generic, Show)

-- deriveIdentifierInstances ''OrganizationId

newtype TenantOrganizationId = TenantOrganizationId
  { _getTenantOrganizationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''TenantOrganizationId

newtype BusinessAddressId = BusinessAddressId
  { _getBusinessAddressId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''BusinessAddressId

newtype PassApplicationId = PassApplicationId
  { _getPassApplicationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''PassApplicationId

newtype FromLocationId = FromLocationId
  { _getFromLocationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''FromLocationId

newtype ToLocationId = ToLocationId
  { _getToLocationId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ToLocationId

newtype AssignedTo = AssignedTo
  { _getAssignedTo :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''AssignedTo

-- newtype LocationId = LocationId
--   { _getLocationId :: Text
--   }
--   deriving (Generic, Show)

-- deriveIdentifierInstances ''LocationId

newtype QuotaId = QuotaId
  { _getQuotaId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''QuotaId

newtype BlacklistedBy = BlacklistedBy
  { _getBlacklistedBy :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''BlacklistedBy

newtype CustomerDetailId = CustomerDetailId
  { _getCustomerDetailId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''CustomerDetailId

newtype PassId = PassId
  { _getPassId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''PassId

newtype UserId = UserId
  { _getUserId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''UserId

newtype RegistrationTokenId = RegistrationTokenId
  { _getRegistrationTokenId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''RegistrationTokenId

newtype BlacklistId = BlacklistId
  { _getBlacklistId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''BlacklistId

newtype AllocatedQuotaId = AllocatedQuotaId
  { _getAllocatedQuotaId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''AllocatedQuotaId

type RegistrationTokenText = Text

newtype TagId = TagId
  { _getTagId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''TagId

newtype DocumentId = DocumentId
  { _getDocumentId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''DocumentId

instance FromField [DocumentId]

instance FromBackendRow MySQL [DocumentId]

instance HasSqlValueSyntax MysqlValueSyntax [DocumentId]

newtype EntityTagId = EntityTagId
  { _getEntityTagId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''EntityTagId

newtype EntityDocumentId = EntityDocumentId
  { _getEntityDocumentId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''EntityDocumentId

newtype CommentId = CommentId
  { _getCommentId :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''CommentId
