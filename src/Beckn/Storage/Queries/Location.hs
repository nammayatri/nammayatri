{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Location where

import           Database.Beam                ((&&.), (<-.), (==.))
import           EulerHS.Prelude              hiding (id)

import qualified Beckn.Storage.Queries        as DB
import qualified Beckn.Types.Common           as Common
import qualified Beckn.Types.Storage.DB       as DB
import qualified Beckn.Types.Storage.Location as Storage
import qualified Database.Beam                as B
import qualified EulerHS.Language             as L
import qualified EulerHS.Types                as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.becknDb

findLocation :: Text -> L.Flow (T.DBResult (Maybe Storage.Location))
findLocation id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = (_id ==. B.val_ id)


findByLocation :: Common.LocationType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int ->  L.Flow (Maybe Storage.Location)
findByLocation locType (Just dist) (Just city) (Just state) (Just country) (Just ward) (Just pin) = do
  DB.findOne dbTable (predicate locType dist city state country ward pin) >>=
   either DB.throwDBError pure
  where
    predicate locType dist city state country ward pin Storage.Location {..} = -- B.val_ True
        (  (_type ==. B.val_  (show $ locType))
        &&. (_district ==. B.val_ dist))

findByLocation locType _ _ _ _ _ _ = pure Nothing
