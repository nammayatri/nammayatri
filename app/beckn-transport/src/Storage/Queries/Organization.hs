{-# LANGUAGE OverloadedLabels #-}

module Storage.Queries.Organization where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import qualified Beckn.Types.Storage.Organization as Storage
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Time
import Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.OrganizationT)
dbTable = DB._organization DB.transporterDb

create :: Storage.Organization -> Flow ()
create Storage.Organization {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..})
    >>= either DB.throwDBError pure

verifyToken :: RegToken -> Flow Storage.Organization
verifyToken regToken = do
  L.logInfo "verifying token" $ show regToken
  DB.findOne dbTable (predicate regToken)
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "UNAUTHENTICATED_USER"
  where
    predicate regToken Storage.Organization {..} = _apiKey ==. B.val_ (Just regToken)

findOrganizationById :: OrganizationId -> Flow Storage.Organization
findOrganizationById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
    >>= fromMaybeM400 "INVALID_ORG_ID"
  where
    predicate Storage.Organization {..} = _id ==. B.val_ id

listOrganizations ::
  Maybe Int ->
  Maybe Int ->
  [Storage.OrganizationType] ->
  [Storage.Status] ->
  Flow [Storage.Organization]
listOrganizations mlimit moffset oType status =
  DB.findAllWithLimitOffsetWhere dbTable (predicate oType status) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = toInteger $ fromMaybe 100 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Organization {..} = B.desc_ _createdAt
    predicate oType status Storage.Organization {..} =
      foldl
        (&&.)
        (B.val_ True)
        [ _status `B.in_` (B.val_ <$> status) ||. complementVal status,
          _type `B.in_` (B.val_ <$> oType) ||. complementVal oType
        ]

complementVal l
  | null l = B.val_ True
  | otherwise = B.val_ False

update ::
  OrganizationId ->
  Storage.Status ->
  Flow (T.DBResult ())
update id status = do
  (currTime :: LocalTime) <- getCurrentTimeUTC
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
  where
    predicate id Storage.Organization {..} = _id ==. B.val_ id
    setClause status currTime Storage.Organization {..} =
      mconcat
        [ _updatedAt <-. B.val_ currTime,
          _status <-. B.val_ status
        ]

updateOrganizationRec :: Storage.Organization -> Flow ()
updateOrganizationRec org =
  DB.update dbTable (setClause org) (predicate $ org ^. #_id)
    >>= either DB.throwDBError pure
  where
    setClause org Storage.Organization {..} =
      mconcat
        [ _name <-. B.val_ (Storage._name org),
          _description <-. B.val_ (Storage._description org),
          _headCount <-. B.val_ (Storage._headCount org),
          _enabled <-. B.val_ (Storage._enabled org),
          _updatedAt <-. B.val_ (Storage._updatedAt org),
          _fromTime <-. B.val_ (Storage._fromTime org)
        ]
    predicate id Storage.Organization {..} = _id ==. B.val_ id
