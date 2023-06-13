{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.QuoteSpecialZone where

import qualified Database.Beam as B
import Database.Beam.Postgres
import Domain.Types.QuoteSpecialZone
import Domain.Types.SearchRequestSpecialZone
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import Sequelize
import qualified Sequelize as Se
import qualified Storage.Beam.QuoteSpecialZone as BeamQSZ
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

-- create :: QuoteSpecialZone -> SqlDB ()
-- create quote = Esq.runTransaction $
--   withFullEntity quote $ \(quoteT, (fareParams', fareParamsDetais)) -> do
--     Esq.create' fareParams'
--     case fareParamsDetais of
--       FareParamsT.ProgressiveDetailsT fppdt -> Esq.create' fppdt
--       FareParamsT.SlabDetailsT -> return ()
--     Esq.create' quoteT

create :: L.MonadFlow m => QuoteSpecialZone -> m ()
create quote = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbConf' -> do
      SQFP.create quote.fareParams
      void $ KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainQuoteSpecialZoneToBeam quote)
    Nothing -> pure ()

-- countAllByRequestId :: Transactionable m => Id SearchRequestSpecialZone -> m Int32
-- countAllByRequestId searchReqId = do
--   fmap (fromMaybe 0) $
--     Esq.findOne $ do
--       dQuote <- from $ table @QuoteSpecialZoneT
--       where_ $
--         dQuote ^. QuoteSpecialZoneSearchRequestId ==. val (toKey searchReqId)
--       pure (countRows @Int32)

countAllByRequestId :: L.MonadFlow m => Id SearchRequestSpecialZone -> m Int
countAllByRequestId searchReqID = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  conn <- L.getOrInitSqlConn (fromJust dbConf)
  case conn of
    Right c -> do
      resp <-
        L.runDB c $
          L.findRow $
            B.select $
              B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                B.filter_' (\(BeamQSZ.QuoteSpecialZoneT {..}) -> searchRequestId B.==?. B.val_ (getId searchReqID)) $
                  B.all_ (meshModelTableEntity @BeamQSZ.QuoteSpecialZoneT @Postgres @(DatabaseWith BeamQSZ.QuoteSpecialZoneT))
      pure (either (const 0) (fromMaybe 0) resp)
    Left _ -> pure 0

-- baseQuoteSpecialZoneQuery ::
--   From
--     ( SqlExpr (Entity QuoteSpecialZoneT)
--         :& SqlExpr (Entity Fare.FareParametersT)
--     )
-- baseQuoteSpecialZoneQuery =
--   table @QuoteSpecialZoneT
--     `innerJoin` table @Fare.FareParametersT
--       `Esq.on` ( \(rb :& farePars) ->
--                    rb ^. QuoteSpecialZoneFareParametersId ==. farePars ^. Fare.FareParametersTId
--                )

findById :: (L.MonadFlow m) => Id QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
findById (Id dQuoteId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      sR <- KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamQSZ.id $ Se.Eq dQuoteId]
      case sR of
        Right (Just x) -> transformBeamQuoteSpecialZoneToDomain x
        _ -> pure Nothing
    Nothing -> pure Nothing

transformBeamQuoteSpecialZoneToDomain :: L.MonadFlow m => BeamQSZ.QuoteSpecialZone -> m (Maybe QuoteSpecialZone)
transformBeamQuoteSpecialZoneToDomain BeamQSZ.QuoteSpecialZoneT {..} = do
  fp <- BeamQFP.findById (Id fareParametersId)
  if isJust fp
    then
      pure $
        Just
          QuoteSpecialZone
            { id = Id id,
              searchRequestId = Id searchRequestId,
              providerId = Id providerId,
              vehicleVariant = vehicleVariant,
              distance = distance,
              estimatedFinishTime = estimatedFinishTime,
              createdAt = createdAt,
              updatedAt = updatedAt,
              validTill = validTill,
              estimatedFare = estimatedFare,
              fareParams = fromJust fp -- to take a default value?
            }
    else pure Nothing

transformDomainQuoteSpecialZoneToBeam :: QuoteSpecialZone -> BeamQSZ.QuoteSpecialZone
transformDomainQuoteSpecialZoneToBeam QuoteSpecialZone {..} =
  BeamQSZ.QuoteSpecialZoneT
    { BeamQSZ.id = getId id,
      BeamQSZ.searchRequestId = getId searchRequestId,
      BeamQSZ.providerId = getId providerId,
      BeamQSZ.vehicleVariant = vehicleVariant,
      BeamQSZ.distance = distance,
      BeamQSZ.estimatedFinishTime = estimatedFinishTime,
      BeamQSZ.createdAt = createdAt,
      BeamQSZ.updatedAt = updatedAt,
      BeamQSZ.validTill = validTill,
      BeamQSZ.estimatedFare = estimatedFare,
      BeamQSZ.fareParametersId = getId fareParams.id
    }
