{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Role where

import qualified Database.Beam as B
import Domain.Types.Role as Role
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common
import Storage.Beam.Common as SBC
import "lib-dashboard" Storage.Queries.Role ()

-- findAllWithLimitOffset ::
--   Transactionable m =>
--   Maybe Integer ->
--   Maybe Integer ->
--   Maybe Text ->
--   m [Role]
-- findAllWithLimitOffset mbLimit mbOffset mbSearchString = do
--   let limitVal = fromIntegral $ fromMaybe 10 mbLimit
--       offsetVal = fromIntegral $ fromMaybe 0 mbOffset
--   Esq.findAll $ do
--     role <- from $ table @RoleT
--     where_ $
--       Esq.whenJust_ mbSearchString (filterBySearchString role)
--     orderBy [asc $ role ^. RoleName]
--     limit limitVal
--     offset offsetVal
--     pure role
--   where
--     filterBySearchString role searchStr = do
--       let likeSearchStr = (%) ++. val searchStr ++. (%)
--       role ^. RoleName
--         `ilike` likeSearchStr

findAllWithLimitOffset ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  m [Role]
findAllWithLimitOffset mbLimit mbOffset mbSearchString = do
  let limitVal = fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (limitVal) $
          B.offset_ (offsetVal) $
            B.orderBy_ (\role -> B.desc_ role.name) $
              B.filter_'
                ( \(role) ->
                    maybe (B.sqlBool_ $ B.val_ False) (\searchStr -> B.sqlBool_ (role.name `B.like_` B.val_ ("%" <> searchStr <> "%"))) mbSearchString
                )
                do
                  role <- B.all_ (SBC.role SBC.atlasDB)
                  pure role
  case res of
    Right res' -> do
      let m' = res'
      m <- catMaybes <$> mapM fromTType' m'
      pure m
    Left _ -> pure []
