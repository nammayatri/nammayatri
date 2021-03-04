{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Organization as Storage
import Beckn.Utils.Common
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrganizationT))
getDbTable =
  DB._organization . DB.transporterDb <$> getSchemaName

create :: Storage.Organization -> Flow ()
create Storage.Organization {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})
    >>= either DB.throwDBError pure

verifyToken :: RegToken -> Flow Storage.Organization
verifyToken regToken = do
  dbTable <- getDbTable
  logInfo "verifying token" $ show regToken
  DB.findOne dbTable (predicate regToken)
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "UNAUTHENTICATED_ORGANIZATION"
  where
    predicate token Storage.Organization {..} = _apiKey ==. B.val_ (Just token)

findOrganizationById :: ID Storage.Organization -> Flow Storage.Organization
findOrganizationById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_ORG_ID"
  where
    predicate Storage.Organization {..} = _id ==. B.val_ id

findOrganizationByShortId :: ShortID Storage.Organization -> Flow (Maybe Storage.Organization)
findOrganizationByShortId shortId = do
  dbTable <- getDbTable
  DB.findOne dbTable (\Storage.Organization {..} -> _shortId ==. B.val_ shortId)
    >>= either DB.throwDBError pure

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  Flow [Storage.Organization]
listOrganizations mlimit moffset oType status = do
  dbTable <- getDbTable
  DB.findAllWithLimitOffsetWhere dbTable predicate limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Organization {..} = B.desc_ _createdAt
    predicate Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status,
          _domain ==. B.val_ (Just Storage.MOBILITY),
          _type `B.in_` (B.val_ <$> oType) ||. complementVal oType,
          _enabled ==. B.val_ True
        ]

loadAllProviders :: Flow [Storage.Organization]
loadAllProviders = do
  dbTable <- getDbTable
  DB.findAllOrErr dbTable predicate
  where
    predicate Storage.Organization {..} =
      _status ==. B.val_ Storage.APPROVED
        &&. _domain ==. B.val_ (Just Storage.MOBILITY)
        &&. _type ==. B.val_ Storage.PROVIDER
        &&. _enabled ==. B.val_ True

complementVal :: (Container t, B.SqlValable p, B.HaskellLiteralForQExpr p ~ Bool) => t -> p
complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  ID Storage.Organization ->
  Storage.Status ->
  Flow (T.DBResult ())
update id status = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate oid Storage.Organization {..} = _id ==. B.val_ oid
    setClause scStatus currTime Storage.Organization {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ scStatus
        ]

updateOrganizationRec :: Storage.Organization -> Flow ()
updateOrganizationRec org = do
  dbTable <- getDbTable
  DB.update dbTable (setClause org) (predicate $ org ^. #_id)
    >>= either DB.throwDBError pure
  where
    setClause sOrg Storage.Organization {..} =
      mconcat
        [ _name <-. B.val_ (Storage._name sOrg),
          _description <-. B.val_ (Storage._description sOrg),
          _headCount <-. B.val_ (Storage._headCount sOrg),
          _enabled <-. B.val_ (Storage._enabled sOrg),
          _updatedAt <-. B.val_ (Storage._updatedAt sOrg),
          _fromTime <-. B.val_ (Storage._fromTime sOrg)
        ]
    predicate id Storage.Organization {..} = _id ==. B.val_ id

findOrgByApiKey :: APIKey -> Flow (Maybe Storage.Organization)
findOrgByApiKey apiKey = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Organization {..} =
      _apiKey ==. B.val_ (Just apiKey)

findOrgByCbUrl :: BaseUrl -> Flow Storage.Organization
findOrgByCbUrl url = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_BAP_URI"
  where
    predicate Storage.Organization {..} = _callbackUrl ==. B.val_ (Just url)

findOrgByShortId :: ShortID Storage.Organization -> Flow Storage.Organization
findOrgByShortId shortId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_SHORT_ID"
  where
    predicate Storage.Organization {..} = _shortId ==. B.val_ shortId
