module Storage.Queries.RoleExtra where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.Role
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Common as SBC
import Storage.Queries.OrphanInstances.Role ()

-- Beam query for listing roles with search and pagination
findAllRolesWithLimitOffset ::
  (BeamFlow m r) =>
  Kernel.Prelude.Maybe Data.Text.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
  m [Domain.Types.Role.Role]
findAllRolesWithLimitOffset mbSearchString mbLimit mbOffset = do
  let limitVal = fromMaybe 10 mbLimit
      offsetVal = fromMaybe 0 mbOffset
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ limitVal $
          B.offset_ offsetVal $
            B.orderBy_ (\role -> B.asc_ role.name) $
              B.filter_'
                ( \role ->
                    maybe (B.sqlBool_ $ B.val_ True) (\searchStr -> B.sqlBool_ (role.name `B.like_` B.val_ ("%" <> searchStr <> "%"))) mbSearchString
                )
                do
                  B.all_ (SBC.role SBC.atlasDB)
  case res of
    Right res' -> do
      catMaybes <$> mapM fromTType' res'
    Left _ -> pure []
