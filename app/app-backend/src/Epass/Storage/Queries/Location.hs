{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.Location where

import App.Types
import qualified Beckn.Types.Storage.Location as BTL
import qualified Data.Text as T
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import Epass.Types.Common
import qualified Epass.Types.Common as Common
import qualified Epass.Types.Storage.DB as DB
import qualified Epass.Types.Storage.Location as Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import qualified Storage.Queries as DB

dbTable :: B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.becknDb

findLocation :: Text -> Flow (T.DBResult (Maybe Storage.Location))
findLocation id =
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

-- :TODO complete this function
findByLocation :: BTL.LocationType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Flow (Maybe Storage.Location)
findByLocation locType dist city state country ward pin =
  DB.findOne dbTable (predicate locType dist city state country ward pin)
    >>= either DB.throwDBError pure
  where
    predicate locType dist city state country ward pin Storage.Location {..} =
      -- B.val_ True
      (_type ==. B.val_ locType)
        &&. (_district ==. B.val_ dist)
findByLocation locType _ _ _ _ _ _ = pure Nothing

findLocationWithErr :: Text -> Flow Storage.Location
findLocationWithErr id =
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.Location {..} = _id ==. B.val_ id

findByStOrDistrict ::
  Maybe Int ->
  Maybe Int ->
  LocateBy ->
  Text ->
  Flow [Storage.Location]
findByStOrDistrict moffset mlimit filterByT filterBy =
  ( DB.run
      . L.findRows
      . B.select
      . B.filter_ (predicate filterByT filterBy)
      . B.offset_ offset
      . B.limit_ limit
      . B.orderBy_ orderByDesc
      . B.all_
      $ dbTable
  )
    >>= either DB.throwDBError pure
  where
    predicate LDISTRICT filterBy Storage.Location {..} =
      _district ==. B.val_ (Just filterBy)
    predicate LSTATE filterBy Storage.Location {..} =
      _state ==. B.val_ (Just filterBy)
    predicate LCITY filterBy Storage.Location {..} =
      _city ==. B.val_ (Just filterBy)
    predicate LWARD filterBy Storage.Location {..} =
      _ward ==. B.val_ (Just filterBy)
    predicate LPINCODE filterBy Storage.Location {..} =
      _pincode ==. B.val_ (read $ T.unpack filterBy)
    limit = toInteger $ fromMaybe 10 mlimit
    offset = toInteger $ fromMaybe 0 moffset
    orderByDesc Storage.Location {..} = B.desc_ _createdAt
