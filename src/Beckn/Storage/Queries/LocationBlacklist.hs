{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.LocationBlacklist where

import           Database.Beam                         ((&&.), (<-.), (==.))
import           EulerHS.Prelude                       hiding (id)

import qualified Beckn.Storage.Queries                 as DB
import qualified Beckn.Storage.Queries                 as DB
import qualified Beckn.Types.API.LocationBlacklist     as API
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.DB                as DB
import qualified Beckn.Types.Storage.LocationBlacklist as Storage
import qualified Beckn.Types.Storage.LocationBlacklist as Storage

import           Beckn.Utils.Common
import           Data.Time
import           Data.Time.LocalTime
import qualified Database.Beam                         as B
import qualified Database.Beam                         as B
import qualified EulerHS.Language                      as L
import qualified EulerHS.Language                      as L
import qualified EulerHS.Types                         as T
import qualified EulerHS.Types                         as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationBlacklistT)
dbTable = DB._locationBlacklist DB.becknDb

create :: Storage.LocationBlacklist -> L.Flow ()
create Storage.LocationBlacklist {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.LocationBlacklist {..}) >>=
  either DB.throwDBError pure

findById :: LocationBlacklistId -> L.Flow (T.DBResult (Maybe Storage.LocationBlacklist))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.LocationBlacklist {..} = (_id ==. B.val_ id)


update ::
  LocationBlacklistId
  -> API.UpdateReq
  -> L.Flow (T.DBResult ())
update id API.UpdateReq {..} = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update dbTable
    (setClause _remarks _TenantOrganizationId _info _type  _ward _district _city _state _country _pincode _bound currTime)
    (predicate id)
  where
    predicate id Storage.LocationBlacklist {..} = _id ==. B.val_ id
    setClause remarksM tenantOrganizationIdM infoM typeM wardM districtM cityM stateM countryM pincodeM boundM currTime Storage.LocationBlacklist {..} =
      mconcat ([_updatedAt <-. B.val_ currTime
              ]
              <> maybe [] (\x -> [ _remarks <-. B.val_ x ]) remarksM
              <> maybe ([]) (return . (_district <-.) . B.val_ . Just) districtM
              <> maybe ([]) (return . (_TenantOrganizationId <-.) . B.val_ . Just) tenantOrganizationIdM
              <> maybe ([]) (return . (_ward <-.) . B.val_ . Just) wardM
              <> maybe ([]) (return . (_info <-.) .  B.val_ . Just) infoM
              <> maybe ([]) (return . (_city <-.) .  B.val_ . Just) cityM
              <> maybe ([]) (return . (_state <-.) .  B.val_ . Just) stateM
              <> maybe ([]) (return . (_country <-.) .  B.val_) countryM
              <> maybe ([]) (return . (_pincode <-.) . B.val_ . Just) pincodeM
              <> maybe ([]) (return . (_bound <-.) . B.val_ . Just) boundM
             )
