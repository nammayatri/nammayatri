{-# LANGUAGE TypeApplications #-}

module Lib.Yudhishthira.TypesTH (generateGenericDefault, generateGenericDefaultWithOverrides, GenericDefaults (..)) where

import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT
import qualified Data.Time
import qualified Data.Time.LocalTime
import qualified Data.Vector
import Kernel.Prelude hiding (Type)
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Language.Haskell.TH
import qualified Servant.Client.Core

class GenericDefaults a where
  genDef :: Proxy a -> [a]

-- | Canonical instance for a widely-shared enum. Without it, multiple config splices
-- (across different modules, e.g. TransporterConfig and UiDriverConfig) each auto-generate
-- their own orphan @GenericDefaults DeviceType@, which overlap when those modules are
-- imported together. Providing one here (in the class's own module, so it is in scope wherever
-- @generateGenericDefault@ is used) makes every splice reuse it instead of regenerating.
-- Returns @[IOS]@ (first constructor only) to exactly match what the auto-generated enum
-- instance produced before, avoiding any change to existing default values.
instance GenericDefaults Kernel.Types.Version.DeviceType where
  genDef _ = [Kernel.Types.Version.IOS]

checkInstance :: Name -> Type -> Q Bool
checkInstance className typ = isInstance className [typ]

getFieldDefaultValues :: Name -> Q (Exp, [Dec])
getFieldDefaultValues fieldTypeName = do
  let resultFromPrimitives = go
      proxyN = mkName "Proxy"
  if resultFromPrimitives == ListE []
    then do
      --let nestedIns = []
      yes <- checkInstance ''GenericDefaults (ConT fieldTypeName)
      nestedIns <-
        if yes
          then pure []
          else generateGenericDefault fieldTypeName
      let funcCall = AppE (VarE 'genDef) (AppTypeE (ConE proxyN) (ConT fieldTypeName))
      return (funcCall, nestedIns)
    else return (resultFromPrimitives, [])
  where
    go
      | fieldTypeName == ''Text = ListE [LitE (StringL "defaultText")]
      | fieldTypeName == ''String = ListE [LitE (StringL "defaultText")]
      | fieldTypeName == ''Int = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Integer = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Double = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''Float = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''HighPrecMoney = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''HighPrecMeters = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''Meters = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Seconds = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Centesimal = ListE [LitE (IntegerL 1)]
      | fieldTypeName == ''Rational = ListE [LitE (RationalL 1.0)]
      | fieldTypeName == ''Bool = ListE [ConE 'True]
      | fieldTypeName == ''UTCTime = ListE [AppE (AppE (ConE 'Data.Time.UTCTime) (AppE (VarE 'read) (LitE (StringL "2000-10-10")))) (LitE (IntegerL 0))]
      | fieldTypeName == ''BaseUrl = ListE [AppE (AppE (AppE (AppE (ConE 'BaseUrl) (ConE 'Servant.Client.Core.Https)) (LitE (StringL "example.com"))) (LitE (IntegerL 443))) (LitE (StringL "/this"))]
      | fieldTypeName == ''Kernel.Types.Id.Id = ListE [LitE (StringL "random-1000-4000-8000-uuid")]
      | fieldTypeName == ''Kernel.Types.Id.ShortId = ListE [LitE (StringL "randomShortId")]
      | fieldTypeName == ''NominalDiffTime = ListE [AppE (VarE 'fromInteger) (LitE (IntegerL 1))]
      | fieldTypeName == ''Data.Time.LocalTime.TimeOfDay = ListE [AppE (AppE (AppE (ConE 'Data.Time.LocalTime.TimeOfDay) (LitE (IntegerL 11))) (LitE (IntegerL 10))) (LitE (IntegerL 10))]
      | fieldTypeName == ''Object =
        ListE
          [ AppE
              (VarE 'Data.Aeson.KeyMap.fromList)
              ( ListE
                  [ TupE [Just (LitE (StringL "z")), Just (LitE (StringL "someString"))],
                    TupE [Just (LitE (StringL "a")), Just (LitE (StringL "someString"))]
                  ]
              )
          ]
      | otherwise = ListE []

-- below just to ease the process a little
generateGenericDefault :: Name -> Q [Dec]
generateGenericDefault = generateGenericDefaultWithOverrides []

generateGenericDefaultWithOverrides :: [(String, [String])] -> Name -> Q [Dec]
generateGenericDefaultWithOverrides overrides typeName = do
  res <- reify typeName
  result <- case res of
    -- [(Ghci1.val,Bang NoSourceUnpackedness NoSourceStrictness,AppT (AppT (ConT Data.HashMap.Internal.HashMap) (ConT Data.Text.Internal.Text)) (ConT Data.Text.Internal.Text))]
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> generateGenericDefaultForRecords overrides typeName conName fields
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) []) -> generateGenericDefaultForRecords overrides typeName conName fields
    TyConI (DataD _ _ _ _ enums _) -> generateGenericDefaultForEnums overrides typeName enums
    TyConI (NewtypeD _ _ _ _ enums _) -> generateGenericDefaultForEnums overrides typeName [enums]
    -- Type synonyms (e.g. HKD aliases like `type Foo = FooD 'Safe`): resolve to the
    -- underlying applied data type, substitute the type arguments and generate the
    -- instance for the fully-applied head (`FooD 'Safe`).
    TyConI (TySynD _n _ rhs) ->
      case unapplyType rhs of
        (ConT realName, args) -> generateGenericDefaultForApplied overrides rhs realName args
        _ -> generateGenericDefaultForEnums overrides typeName []
    _ -> fail $ "can't generate GenericDefaults for type: " ++ show res
  -- runIO $ putStrLn (pprint result)
  pure result

-- | Generate a 'GenericDefaults' instance for a fully-applied data type
-- (e.g. @FooD 'Safe@). @headTy@ is the instance head, @realName@ the underlying
-- data constructor name and @args@ the applied type arguments.
generateGenericDefaultForApplied :: [(String, [String])] -> Type -> Name -> [Type] -> Q [Dec]
generateGenericDefaultForApplied overrides headTy realName args = do
  info <- reify realName
  case info of
    TyConI (DataD _ _ tvs _ [RecC conName fields] _) ->
      generateGenericDefaultForRecordsTy overrides headTy conName (map (substField (mkSubst tvs args)) fields)
    TyConI (NewtypeD _ _ tvs _ (RecC conName fields) _) ->
      generateGenericDefaultForRecordsTy overrides headTy conName (map (substField (mkSubst tvs args)) fields)
    TyConI (DataD _ _ tvs _ enums _) ->
      generateGenericDefaultForEnumsTy overrides headTy (map (substCon (mkSubst tvs args)) enums)
    TyConI (NewtypeD _ _ tvs _ enum _) ->
      generateGenericDefaultForEnumsTy overrides headTy [substCon (mkSubst tvs args) enum]
    _ -> pure []

-- | Split a (possibly applied) type into its head constructor and arguments.
unapplyType :: Type -> (Type, [Type])
unapplyType = go []
  where
    go acc (AppT f x) = go (x : acc) f
    go acc (SigT t _) = go acc t
    go acc (ParensT t) = go acc t
    go acc t = (t, acc)

tyVarName :: TyVarBndr flag -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n

mkSubst :: [TyVarBndr flag] -> [Type] -> [(Name, Type)]
mkSubst tvs args = zip (map tyVarName tvs) args

substType :: [(Name, Type)] -> Type -> Type
substType sub = go
  where
    go (VarT n) = fromMaybe (VarT n) (lookup n sub)
    go (AppT a b) = AppT (go a) (go b)
    go (AppKindT t k) = AppKindT (go t) k
    go (SigT t k) = SigT (go t) k
    go (InfixT a n b) = InfixT (go a) n (go b)
    go (ParensT t) = ParensT (go t)
    go t = t

substField :: [(Name, Type)] -> VarBangType -> VarBangType
substField sub (n, b, t) = (n, b, substType sub t)

substCon :: [(Name, Type)] -> Con -> Con
substCon sub con = case con of
  NormalC n bts -> NormalC n (map (\(b, t) -> (b, substType sub t)) bts)
  RecC n vbts -> RecC n (map (substField sub) vbts)
  InfixC (b1, t1) n (b2, t2) -> InfixC (b1, substType sub t1) n (b2, substType sub t2)
  ForallC vs cx c -> ForallC vs cx (substCon sub c)
  GadtC ns bts t -> GadtC ns (map (\(b, t') -> (b, substType sub t')) bts) (substType sub t)
  RecGadtC ns vbts t -> RecGadtC ns (map (substField sub) vbts) (substType sub t)

generateGenericDefaultDefaultForType :: Type -> Q (Exp, [Dec])
generateGenericDefaultDefaultForType fieldType = do
  case fieldType of
    AppT (ConT maybeName) (AppT ListT (AppT (ConT typeOfConstructor) (ConT _rawFieldType))) | maybeName == ''Maybe && typeOfConstructor == ''Kernel.Types.Id.Id -> do
      fieldGenericDefaults <- getFieldDefaultValues typeOfConstructor
      let fieldDefaultValues = fst fieldGenericDefaults
      pure (ListE [AppE (ConE 'Just) (fieldDefaultValues)], snd fieldGenericDefaults)
    AppT (ConT maybeName) (AppT (ConT typeOfConstructor) (ConT _rawFieldType)) | maybeName == ''Maybe && typeOfConstructor == ''Kernel.Types.Id.Id -> do
      fieldGenericDefaults <- getFieldDefaultValues typeOfConstructor
      let fieldDefaultValues = fst fieldGenericDefaults
      let allValues = AppE (AppE (VarE 'map) (ConE 'Just)) fieldDefaultValues
      pure (allValues, snd fieldGenericDefaults)
    AppT (ConT maybeName) (AppT ListT (ConT rawFieldType)) | maybeName == ''Maybe -> do
      fieldGenericDefaults <- getFieldDefaultValues rawFieldType
      let fieldDefaultValues = fst fieldGenericDefaults
      let allValues = ListE [AppE (ConE 'Just) fieldDefaultValues]
      pure (allValues, snd fieldGenericDefaults)
    AppT (ConT maybeName) (ConT rawFieldType) | maybeName == ''Maybe -> do
      fieldGenericDefaults <- getFieldDefaultValues rawFieldType
      let fieldDefaultValues = fst fieldGenericDefaults
      let allValues = AppE (AppE (VarE 'map) (ConE 'Just)) fieldDefaultValues
      pure (allValues, snd fieldGenericDefaults)
    AppT (ConT typeOfConstructor) (ConT _rawFieldType) | typeOfConstructor == ''Kernel.Types.Id.ShortId -> do
      fieldGenericDefaults <- getFieldDefaultValues typeOfConstructor
      let fieldDefaultValues = fst fieldGenericDefaults
      pure (fieldDefaultValues, snd fieldGenericDefaults)
    AppT (ConT typeOfConstructor) (ConT _rawFieldType) | typeOfConstructor == ''Kernel.Types.Id.Id -> do
      fieldGenericDefaults <- getFieldDefaultValues typeOfConstructor
      let fieldDefaultValues = fst fieldGenericDefaults
      pure (fieldDefaultValues, snd fieldGenericDefaults)
    AppT ListT (ConT rawFieldType) -> do
      fieldGenericDefaults <- getFieldDefaultValues rawFieldType
      let fieldDefaultValues = fst fieldGenericDefaults
      let allValues = ListE [fieldDefaultValues]
      pure (allValues, snd fieldGenericDefaults)
    ConT rawFieldType -> do
      fieldGenericDefaults <- getFieldDefaultValues rawFieldType
      let fieldDefaultValues = fst fieldGenericDefaults
      pure (fieldDefaultValues, snd fieldGenericDefaults)
    AppT (ConT vecConst) (ConT rawFieldType) | vecConst == ''Data.Vector.Vector -> do
      fieldGenericDefaults <- getFieldDefaultValues rawFieldType
      let fieldDefaultValues = fst fieldGenericDefaults
      pure (ListE [AppE (VarE 'Data.Vector.fromList) fieldDefaultValues], snd fieldGenericDefaults)
    AppT ListT (AppT (AppT (TupleT 2) (ConT typeName1)) (ConT typeName2)) -> do
      fieldGenericDefaults1 <- getFieldDefaultValues typeName1
      fieldGenericDefaults2 <- getFieldDefaultValues typeName2
      let fieldDefaultValues1' = fst fieldGenericDefaults1
      let fieldDefaultValues2' = fst fieldGenericDefaults2
      let combinedDes = snd fieldGenericDefaults1 ++ snd fieldGenericDefaults2
      let fieldDefaultValues1 = case fieldDefaultValues1' of
            ListE elems -> elems
            val -> fail $ "broke in tuple expected a list" <> show val
          fieldDefaultValues2 = case fieldDefaultValues2' of
            ListE elems -> elems
            val -> fail $ "broke in tuple expected a list" <> show val
      let elements = ListE [ListE $ zipWith (\k v -> TupE [Just k, Just v]) fieldDefaultValues1 fieldDefaultValues2]

      pure (elements, combinedDes)
    AppT (AppT (ConT hashMapName) type1) type2 | hashMapName == ''HashMap.HashMap -> do
      domain1Defaults <- generateGenericDefaultDefaultForType type1
      domain2Defaults <- generateGenericDefaultDefaultForType type2
      let keyExp = fst domain1Defaults
      let valueExp = fst domain2Defaults
      let keyExpList = case keyExp of
            ListE elems -> elems
            AppE (VarE fn) arg -> do
              let evaluatedExp = AppE (VarE fn) arg
              [AppE (VarE 'head) evaluatedExp]
            single -> [single]
      let valueExpList = case valueExp of
            ListE elems -> elems
            AppE (VarE fn) arg -> do
              let evaluatedExp = AppE (VarE fn) arg
              [AppE (VarE 'head) evaluatedExp]
            single -> [single]
      let hashMapExp =
            ListE
              [ AppE
                  (VarE 'HashMap.fromList)
                  (ListE (zipWith (\k v -> TupE [Just k, Just v]) keyExpList valueExpList))
              ]
      let combinedDec = snd domain1Defaults ++ snd domain2Defaults
      pure (hashMapExp, combinedDec)
    -- Strict map field, e.g. `Map Text [VerificationService]`.
    AppT (AppT (ConT mapName) keyT) valT | mapName == ''Map.Map -> do
      domain1Defaults <- generateGenericDefaultDefaultForType keyT
      domain2Defaults <- generateGenericDefaultDefaultForType valT
      let toExpList e = case e of
            ListE elems -> elems
            AppE (VarE fn) arg -> [AppE (VarE 'head) (AppE (VarE fn) arg)]
            single -> [single]
      let keyExpList = toExpList (fst domain1Defaults)
          valueExpList = toExpList (fst domain2Defaults)
      let mapExp = ListE [AppE (VarE 'Map.fromList) (ListE (zipWith (\k v -> TupE [Just k, Just v]) keyExpList valueExpList))]
      pure (mapExp, snd domain1Defaults ++ snd domain2Defaults)
    -- Generic `Maybe t` for inner types not covered by the specific cases above
    -- (e.g. `Maybe (Map ..)`).
    AppT (ConT maybeName) inner | maybeName == ''Maybe -> do
      (vals, decs) <- generateGenericDefaultDefaultForType inner
      pure (AppE (AppE (VarE 'map) (ConE 'Just)) vals, decs)
    a@(_) -> do
      runIO $ putStrLn $ "non supported type: " ++ show a
      fail "Not Supported currently, to add support for your case check this: https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH.html#g:22"

generateGenericDefaultForRecords :: [(String, [String])] -> Name -> Name -> [VarBangType] -> Q [Dec]
generateGenericDefaultForRecords overrides typeName = generateGenericDefaultForRecordsTy overrides (ConT typeName)

generateGenericDefaultForRecordsTy :: [(String, [String])] -> Type -> Name -> [VarBangType] -> Q [Dec]
generateGenericDefaultForRecordsTy overrides headTy conName fields = do
  let typeProxy = mkName "_typeProxy"
  fieldCombinations <- forM fields $ \(fieldName, _, fieldType) -> do
    let fieldName' = DT.unpack $ DL.reverse (DT.splitOn "." $ show fieldName) DL.!! 0
    let fieldNameVar = mkName $ fieldName' <> "Var"
    if elem fieldName' (map fst overrides)
      then pure $ (BindS (VarP fieldNameVar) $ foldl' (\acc (fn, fvs) -> if fn == fieldName' then UInfixE acc (VarE '(<>)) (ListE $ map (\fv -> AppE (VarE 'read) (LitE (StringL fv))) fvs) else acc) (ListE []) overrides, [])
      else do
        (fieldValues, fieldGetAllInstance) <- generateGenericDefaultDefaultForType fieldType
        pure (BindS (VarP fieldNameVar) fieldValues, fieldGetAllInstance)

  let mkTypeValue acc fieldCombination = do
        case fst fieldCombination of
          BindS (VarP fieldNameVar) _ -> pure $ AppE acc (VarE fieldNameVar)
          _ -> fail $ pprint headTy <> ", this shouldn't have happened, generated from previous step. something is wrong with the type :)"

  almostBody <- foldlM mkTypeValue (ConE conName) fieldCombinations
  let allDec = foldl' (\acc fieldCombination -> acc <> (snd fieldCombination)) [] fieldCombinations
  let allfieldDefaultsList = map fst fieldCombinations

  let body = DoE Nothing (allfieldDefaultsList ++ [NoBindS (AppE (VarE 'return) almostBody)])

  let finalBody = UInfixE (AppE (VarE 'take) (LitE (IntegerL 10))) (VarE '($)) body

  -- runIO $ putStrLn $ (pprint finalBody)

  let functionDec = [FunD 'genDef [Clause [VarP typeProxy] (NormalB finalBody) []]]
  let instanceDec = [InstanceD Nothing [] (AppT (ConT ''GenericDefaults) headTy) functionDec]

  return $ DL.nub $ instanceDec <> allDec

generateGenericDefaultForEnums :: [(String, [String])] -> Name -> [Con] -> Q [Dec]
generateGenericDefaultForEnums overrides enumName = generateGenericDefaultForEnumsTy overrides (ConT enumName)

generateGenericDefaultForEnumsTy :: [(String, [String])] -> Type -> [Con] -> Q [Dec]
generateGenericDefaultForEnumsTy _ _ [] = return []
generateGenericDefaultForEnumsTy overrides headTy (firstEnum : _rest) = do
  let typeProxy = mkName "_typeProxy"
  (body, innerDec) <-
    if not $ null overrides
      then pure (foldl' (\accD (_, fvs) -> UInfixE accD (VarE '(<>)) (ListE $ map (\fv -> AppE (VarE 'read) (LitE (StringL fv))) fvs)) (ListE []) overrides, [])
      else do
        foldlF [firstEnum] (ListE [], []) $ \(acc, innerD) enum -> do
          fmap (\(fstV, sndV) -> (UInfixE acc (VarE '(<>)) fstV, innerD <> sndV)) $ do
            case enum of
              NormalC en [(_, ConT wrappedType)] -> do
                (fieldValues, innerD') <- getFieldDefaultValues wrappedType
                pure (AppE (AppE (VarE 'map) (ConE en)) fieldValues, innerD')
              NormalC en [(_, AppT ListT (ConT wrappedType))] -> do
                (fieldValues, innerD') <- getFieldDefaultValues wrappedType
                let x = mkName "x"
                pure (AppE (AppE (VarE 'map) (LamE [VarP x] (AppE (ConE en) (ListE [VarE x])))) fieldValues, innerD')
              NormalC en [(_, AppT (ConT typeWrapper) (ConT wrappedType))] | typeWrapper == ''Maybe -> do
                fieldGenericDefaults <- getFieldDefaultValues wrappedType
                let fieldValues = fst fieldGenericDefaults
                pure ((AppE (AppE (VarE 'map) (UInfixE (ConE en) (VarE '(.)) (ConE 'Just))) fieldValues), snd fieldGenericDefaults)
              NormalC en [(_, AppT (ConT typeWrapper) (ConT _))] | typeWrapper == ''Kernel.Types.Id.Id -> do
                fieldGenericDefaults <- getFieldDefaultValues typeWrapper
                let fieldValues = fst fieldGenericDefaults
                pure ((AppE (AppE (VarE 'map) (UInfixE (ConE en) (VarE '(.)) (ConE 'Kernel.Types.Id.Id))) fieldValues), snd fieldGenericDefaults)
              NormalC en [(_, ConT enumInnerValType), (_, AppT (ConT typeWrapper) (ConT wrappedType))] | typeWrapper == ''Maybe -> do
                enumInnerDefaultVals <- getFieldDefaultValues enumInnerValType
                fieldGenericDefaults <- getFieldDefaultValues wrappedType
                let fstLis = mkName "a"
                    sndLis = mkName "b"
                    almostBody = TupE [Just (VarE fstLis), Just (VarE sndLis)]
                    allCombinations = DoE Nothing [BindS (VarP fstLis) (fst enumInnerDefaultVals), BindS (VarP sndLis) (fst fieldGenericDefaults), NoBindS (AppE (VarE 'return) almostBody)]
                    tupFst = mkName "x"
                    tupSnd = mkName "y"
                pure ((AppE (AppE (VarE 'map) (LamE [TupP [VarP tupFst, VarP tupSnd]] (UInfixE (AppE (ConE en) (VarE tupFst)) (VarE '($)) (AppE (ConE 'Just) (VarE tupSnd))))) allCombinations), snd fieldGenericDefaults <> snd enumInnerDefaultVals)
              NormalC en _ -> pure $ (ListE [ConE en], [])
              _ -> fail $ "expected enum, but got -> " <> show enum
  let finalBody = UInfixE (AppE (VarE 'take) (LitE (IntegerL 10))) (VarE '($)) body
  let functionDec = [FunD 'genDef [Clause [VarP typeProxy] (NormalB finalBody) []]]
  let instanceDec = [InstanceD Nothing [] (AppT (ConT ''GenericDefaults) headTy) functionDec]
  return $ DL.nub $ instanceDec <> innerDec
  where
    foldlF arrV acc fn = foldlM fn acc arrV
