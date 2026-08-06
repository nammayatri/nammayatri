{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PersonExtra where

import qualified Data.List.NonEmpty as NE
import qualified Database.Beam as B
import Domain.Types.Merchant as Merchant
import Domain.Types.MerchantAccess as MerchantAccess
import Domain.Types.Person as Person
import Domain.Types.Role as Role
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, logTagError)
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.MerchantAccess as BeamMA
import qualified Storage.Beam.Person as Beam
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Role as BeamR
import Storage.Queries.MerchantAccess ()
import Storage.Queries.OrphanInstances.Person
import Storage.Queries.OrphanInstances.Role
import Storage.Queries.Role ()

-- Extra code goes here --

findAllPersonsWithLimitOffset ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Int ->
  Int ->
  m [Person.Person]
findAllPersonsWithLimitOffset limit offset = do
  findAllWithOptionsKV
    [Se.Is Beam.id $ Se.Not $ Se.Eq ""]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)

findAllWithLimitOffset ::
  BeamFlow m r =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe (Id Person.Person) ->
  m [(Person.Person, Role.Role, [ShortId Merchant.Merchant], [City.City])]
findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset personId = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ limitVal $
          B.offset_ offsetVal $
            B.orderBy_ (\(person, _, _) -> B.desc_ person.createdAt) $
              B.filter_'
                ( \(person, _role, _) ->
                    ( maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.concat_ [person.firstName, person.lastName] `B.like_` B.val_ ("%" <> searchString <> "%"))) mbSearchString
                        B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchStrDBHash -> person.mobileNumberHash B.==?. B.just_ (B.val_ searchStrDBHash)) mbSearchStrDBHash
                    )
                      B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\defaultPerson -> person.id B.==?. B.val_ (getId defaultPerson)) personId
                )
                $ do
                  person <- B.all_ (SBC.person SBC.atlasDB)
                  role <- B.join_' (SBC.role SBC.atlasDB) (\role -> BeamP.roleId person B.==?. BeamR.id role)
                  merchantAccess <- B.leftJoin_' (B.all_ $ SBC.merchantAccess SBC.atlasDB) (\merchantAccess -> BeamP.id person B.==?. BeamMA.personId merchantAccess)
                  pure (person, role, merchantAccess)
  case res of
    Right res' -> do
      finalRes <- forM res' $ \(person, role, mbMerchantAccess) -> runMaybeT $ do
        p <- MaybeT $ fromTType' person
        r <- MaybeT $ fromTType' role
        ma <- forM mbMerchantAccess (MaybeT . fromTType')
        pure (p, r, ma)
      pure $ groupByPerson $ catMaybes finalRes
    Left err -> do
      logTagError "PersonExtra.findAllWithLimitOffset" $ "Database query failed: " <> show err
      pure []
  where
    limitVal = fromMaybe 100 mbLimit
    offsetVal = fromMaybe 0 mbOffset
    groupByPerson ::
      [(Person.Person, Role.Role, Maybe MerchantAccess.MerchantAccess)] ->
      [(Person.Person, Role.Role, [ShortId Merchant.Merchant], [City.City])]
    groupByPerson inputList =
      map processGroup $ groupByPerson' inputList

    groupByPerson' ::
      [(Person.Person, Role.Role, Maybe MerchantAccess.MerchantAccess)] ->
      [NonEmpty (Person.Person, Role.Role, Maybe MerchantAccess.MerchantAccess)]
    groupByPerson' = NE.groupBy ((==) `on` (\(p, _, _) -> p.id))

    processGroup ::
      NonEmpty (Person.Person, Role.Role, Maybe MerchantAccess.MerchantAccess) ->
      (Person.Person, Role.Role, [ShortId Merchant.Merchant], [City.City])
    processGroup group =
      let (person, role, _) = NE.head group
          merchantAccessList = mapMaybe (\(_, _, ma) -> ma) $ toList group
          cities = merchantAccessList <&> (.operatingCity)
          merchantIds = merchantAccessList <&> MerchantAccess.merchantShortId
       in (person, role, merchantIds, cities)
