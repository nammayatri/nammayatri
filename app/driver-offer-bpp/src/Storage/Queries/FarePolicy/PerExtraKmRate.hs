module Storage.Queries.FarePolicy.PerExtraKmRate
  ( Storage.Queries.FarePolicy.PerExtraKmRate.findAll,
    deleteAll,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.FarePolicy.PerExtraKmRate
import Domain.Types.Organization (Organization)
import Storage.Tabular.FarePolicy.PerExtraKmRate
import Types.Error (FarePolicyError (NoPerExtraKmRate))
import Utils.Common

findAll ::
  ( Transactionable m,
    Monad m,
    MonadThrow m,
    Log m
  ) =>
  Id Organization ->
  m (NonEmpty PerExtraKmRate)
findAll orgId = do
  rez <- Esq.findAll $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)
    orderBy [asc $ perExtraKmRate ^. PerExtraKmRateDistanceRangeStart]
    return perExtraKmRate
  noneEmptyRez <- case rez of
    e : es -> pure $ e :| es
    [] -> throwError NoPerExtraKmRate
  return (getDomainPart <$> noneEmptyRez)

deleteAll :: Id Organization -> SqlDB ()
deleteAll orgId =
  delete' $ do
    perExtraKmRate <- from $ table @PerExtraKmRateT
    where_ $
      perExtraKmRate ^. PerExtraKmRateOrganizationId ==. val (toKey orgId)

getDomainPart :: FullPerExtraKmRate -> PerExtraKmRate
getDomainPart (_, _, domain) = domain
