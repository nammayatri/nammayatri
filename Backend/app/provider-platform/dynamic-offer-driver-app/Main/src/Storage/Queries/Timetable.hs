{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.Queries.Timetable where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.LocalTime (LocalTime (..))
import qualified Database.Esqueleto.Experimental as Ex
import qualified Database.Esqueleto.Internal.Internal as EI
import qualified Database.Esqueleto.PostgreSQL as EPQ
import Database.Persist (unFieldNameDB)
import Database.Persist.SqlBackend
import qualified Domain.Types.RecurringBooking as DRecurringBooking
import Domain.Types.TimeRange (TimeRange)
import Domain.Types.Timetable (Timetable)
import qualified Domain.Types.Timetable as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Storage.Tabular.FarePolicy
import Storage.Tabular.RecurringBooking
import Storage.Tabular.Timetable as STimetable

instance FromTType (TimetableT, RecurringBookingT, FarePolicyT) Domain.UpcomingBooking where
  fromTType (timetableT, bookingT, farePolicyT) = do
    tt <- fromTType @_ @Domain.Timetable timetableT
    DRecurringBooking.SimpleRecurringBooking {..} <- fromTType bookingT
    farePolicy <- fromTType farePolicyT
    pure $ Domain.UpcomingBooking {id = tt.id, pickupTime = tt.pickupTime, ..}

findAllUnscheduledAndActiveDuring :: Transactionable m => TimeRange -> m [Domain.UpcomingBooking]
findAllUnscheduledAndActiveDuring timeRange = Esq.buildDType $
  fmap (fmap $ extractSolidType @Domain.UpcomingBooking) $
    Esq.findAll' $ do
      (tt :& rbt :& fpt) <-
        from $
          table @TimetableT
            `innerJoin` table @RecurringBookingT
            `Esq.on` (\(tt :& rbt) -> tt ^. TimetableRecurringBookingId ==. rbt ^. RecurringBookingTId)
            `innerJoin` table @FarePolicyT
            `Esq.on` (\(_ :& rbt :& fpt) -> rbt ^. RecurringBookingFarePolicyId ==. fpt ^. FarePolicyTId)
      where_ $
        tt ^. TimetableStatus ==. val Domain.Active
          &&. Esq.isNothing (tt ^. TimetableBookingId)
          &&. tt ^. TimetablePickupTime >=. val (localTimeOfDay timeRange.start)
          &&. tt ^. TimetablePickupTime <=. val (localTimeOfDay timeRange.end)
          &&. tt ^. TimetablePickupDate >=. val (localDay timeRange.start)
          &&. tt ^. TimetablePickupDate <=. val (localDay timeRange.end)

      pure (tt, rbt, fpt)

updateTimetablesBookingId :: [Timetable] -> SqlDB ()
updateTimetablesBookingId timetables = do
  let timetableToValues timetable =
        ( val (STimetable.id timetable),
          val (bookingId timetable)
        )
  for_ (NE.nonEmpty $ fmap (timetableToValues . toTType) timetables) $ \timetablesForUpdate -> do
    Esq.update $ \currentTimetable -> do
      (ttId, ttBookingId) <- from $ values timetablesForUpdate
      set
        currentTimetable
        [ TimetableBookingId =. ttBookingId
        ]
      where_ $ currentTimetable ^. TimetableId ==. ttId

-- NOTE: This was copied from esqueleto 3.5.2.3 need to update version
values :: (EI.ToSomeValues a, Ex.ToAliasReference a, Ex.ToAlias a) => NE.NonEmpty a -> Ex.From a
values exprs = Ex.From $ do
  ident <- EI.newIdentFor $ EI.DBName "vq"
  alias <- Ex.toAlias $ NE.head exprs
  ref <- Ex.toAliasReference ident alias
  let aliasIdents =
        mapMaybe
          ( \someVal -> case someVal of
              EI.SomeValue (EI.ERaw aliasMeta _) -> EI.sqlExprMetaAlias aliasMeta
          )
          $ EI.toSomeValues ref
  pure (ref, const $ mkExpr ident aliasIdents)
  where
    someValueToSql :: EI.IdentInfo -> EI.SomeValue -> (TLB.Builder, [PersistValue])
    someValueToSql info (EI.SomeValue expr) = EI.materializeExpr info expr

    mkValuesRowSql :: EI.IdentInfo -> [EI.SomeValue] -> (TLB.Builder, [PersistValue])
    mkValuesRowSql info vs =
      let materialized = someValueToSql info <$> vs
          valsSql = TLB.toLazyText . fst <$> materialized
          params = concatMap snd materialized
       in (TLB.fromLazyText $ "(" <> TL.intercalate "," valsSql <> ")", params)

    -- (VALUES (v11, v12,..), (v21, v22,..)) as "vq"("v1", "v2",..)
    mkExpr :: EI.Ident -> [EI.Ident] -> EI.IdentInfo -> (TLB.Builder, [PersistValue])
    mkExpr valsIdent colIdents info =
      let materialized = mkValuesRowSql info . EI.toSomeValues <$> NE.toList exprs
          (valsSql, params) =
            ( TL.intercalate "," $ map (TLB.toLazyText . fst) materialized,
              concatMap snd materialized
            )
          colsAliases = TL.intercalate "," (map (TLB.toLazyText . EI.useIdent info) colIdents)
       in ( "(VALUES " <> TLB.fromLazyText valsSql <> ") AS "
              <> EI.useIdent info valsIdent
              <> "("
              <> TLB.fromLazyText colsAliases
              <> ")",
            params
          )

insertTimetables :: [Timetable] -> SqlDB ()
insertTimetables timetables = do
  SqlDB $
    lift $
      insertManyOnConflict
        (fmap toTType timetables)
        UniqueBookingPickupDate
        DoNothing

insertManyOnConflict ::
  forall a backend m unique.
  ( PersistEntity a,
    EI.KnowResult unique ~ Unique a,
    EI.FinalResult unique,
    MonadIO m,
    BackendCompatible SqlBackend backend
  ) =>
  [a] ->
  unique ->
  OnConflict a ->
  ReaderT backend m ()
insertManyOnConflict valuesToInsert conflictKey onConflict = do
  sqlBackend <- asks projectBackend
  let info = (sqlBackend, EI.initialIdentState)

      -- Since we don't actually ever look at anything other than the type of the
      -- SqlExpr argument in the sqlInsertInto call we can use laziness to
      -- summon a value with the correct type
      --
      -- NOTE: do not try to force this value as it will cause infinite recursion
      insertionProxy = insertionProxy :: SqlExpr (EI.Insertion a)

      insertIntoFragment = EI.sqlInsertInto info insertionProxy

      (valuesBuilders, valuesValues) = unzip $ fmap mkSqlEntityFragment valuesToInsert

      valuesFragment =
        ( "VALUES " <> EI.uncommas valuesBuilders <> "\n",
          concat valuesValues
        )

      uniqueFieldName =
        EI.uncommas $
          toList $
            TLB.fromText . flip getEscapedRawName sqlBackend . unFieldNameDB . snd
              <$> persistUniqueToFieldNames (EI.finalR conflictKey)

      onConflictFragment =
        ("ON CONFLICT (" <> uniqueFieldName <> ") ", []) <> renderOnConflict onConflict

      (queryBuilder, queryValues) =
        mconcat [insertIntoFragment, valuesFragment, onConflictFragment]
  rawExecute (TL.toStrict $ TLB.toLazyText queryBuilder) queryValues

mkSqlEntityFragment :: (PersistEntity a) => a -> (TLB.Builder, [PersistValue])
mkSqlEntityFragment entityVal =
  let vals = fmap toPersistValue $ toPersistFields entityVal
      builder = EI.parens $ EI.uncommas $ replicate (length vals) "?"
   in (builder, vals)

renderOnConflict :: OnConflict a -> (TLB.Builder, [PersistValue])
renderOnConflict onConflict =
  case onConflict of
    DoNothing -> ("DO NOTHING", [])

data OnConflict a
  = DoNothing
