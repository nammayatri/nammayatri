{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Issues where

import Domain.Types.Issue
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types (MeshError (MKeyNotFound), MeshResult)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Issue as BeamI
import Storage.Tabular.Issue
import Storage.Tabular.Person

-- insertIssue :: Issue -> SqlDB ()
-- insertIssue = do
--   Esq.create

insertIssue :: L.MonadFlow m => Issue -> m (MeshResult ())
insertIssue issue = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamI.IssueT
  updatedMeshConfig <- setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainIssueToBeam issue)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findByCustomerId :: Transactionable m => Id Person -> Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [(Issue, Person)]
findByCustomerId customerId mbLimit mbOffset fromDate toDate = Esq.findAll $ do
  (issues :& person) <-
    from $
      table @IssueT
        `innerJoin` table @PersonT
        `Esq.on` ( \(issues :& person) ->
                     issues ^. IssueCustomerId ==. person ^. PersonTId
                 )
  where_ $
    issues ^. IssueCustomerId ==. val (toKey customerId)
      &&. issues ^. IssueCreatedAt >=. val fromDate
      &&. issues ^. IssueCreatedAt <=. val toDate
  orderBy [desc $ issues ^. IssueCreatedAt]
  limit limitVal
  offset offsetVal
  pure (issues, person)
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

findAllIssue :: Transactionable m => Id Merchant -> Maybe Int -> Maybe Int -> UTCTime -> UTCTime -> m [(Issue, Person)]
findAllIssue merchantId mbLimit mbOffset fromDate toDate = Esq.findAll $ do
  (issues :& person) <-
    from $
      table @IssueT
        `innerJoin` table @PersonT
        `Esq.on` ( \(issues :& person) ->
                     issues ^. IssueCustomerId ==. person ^. PersonTId
                 )
  where_ $
    person ^. PersonMerchantId ==. val (toKey merchantId)
      &&. issues ^. IssueCreatedAt >=. val fromDate
      &&. issues ^. IssueCreatedAt <=. val toDate
  limit limitVal
  offset offsetVal
  pure (issues, person)
  where
    limitVal = min (maybe 10 fromIntegral mbLimit) 10
    offsetVal = maybe 0 fromIntegral mbOffset

transformBeamIssueToDomain :: BeamI.Issue -> Issue
transformBeamIssueToDomain BeamI.IssueT {..} = do
  Issue
    { id = Id id,
      customerId = Id customerId,
      bookingId = Id <$> bookingId,
      contactEmail = contactEmail,
      reason = reason,
      description = description,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainIssueToBeam :: Issue -> BeamI.Issue
transformDomainIssueToBeam Issue {..} =
  BeamI.IssueT
    { BeamI.id = getId id,
      BeamI.customerId = getId customerId,
      BeamI.bookingId = getId <$> bookingId,
      BeamI.contactEmail = contactEmail,
      BeamI.reason = reason,
      BeamI.description = description,
      BeamI.createdAt = createdAt,
      BeamI.updatedAt = updatedAt
    }
