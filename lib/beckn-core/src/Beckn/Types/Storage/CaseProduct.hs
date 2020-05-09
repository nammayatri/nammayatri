{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.CaseProduct where

import           Beckn.Types.App
import           Data.Swagger
import qualified Data.Text                 as T
import           Data.Time.LocalTime
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude
import           Servant.Swagger

data CaseProductStatus = VALID | INPROGRESS | INSTOCK | OUTOFSTOCK
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CaseProductStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL CaseProductStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data CaseProductT f =
  CaseProduct
    { _id                   :: B.C f CaseProductId
    , _caseId               :: B.C f CaseId
    , _productId            :: B.C f ProductsId
    , _quantity             :: B.C f Int
    , _price                :: B.C f Double
    , _status               :: B.C f CaseProductStatus
    , _info                 :: B.C f (Maybe Text)
    , _createdAt            :: B.C f LocalTime
    , _updatedAt            :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)


type CaseProduct = CaseProductT Identity 

type CaseProductPrimaryKey = B.PrimaryKey CaseProductT Identity

instance B.Table CaseProductT where
  data PrimaryKey CaseProductT f = CaseProductPrimaryKey (B.C f CaseProductId)
                               deriving (Generic, B.Beamable)
  primaryKey = CaseProductPrimaryKey . _id

deriving instance Show CaseProduct

deriving instance Eq CaseProduct

instance ToJSON CaseProduct where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON CaseProduct where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToSchema CaseProduct

insertExpression products = insertExpressions [products]

insertExpressions products = B.insertValues products


fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity CaseProductT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }