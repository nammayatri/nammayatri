{-# LANGUAGE TypeApplications #-}

module Lib.Yudhishthira.TypesTH (generateGenericDefault, GenericDefaults (..)) where

import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Time
import qualified Data.Time.LocalTime
import qualified Data.Vector
import Kernel.Prelude hiding (Type)
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Language.Haskell.TH
import qualified Servant.Client.Core

class GenericDefaults a where
  genDef :: Proxy a -> [a]

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
generateGenericDefault typeName = do
  let overrides = []
  res <- reify typeName
  result <- case res of
    -- [(Ghci1.val,Bang NoSourceUnpackedness NoSourceStrictness,AppT (AppT (ConT Data.HashMap.Internal.HashMap) (ConT Data.Text.Internal.Text)) (ConT Data.Text.Internal.Text))]
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> generateGenericDefaultForRecords overrides typeName conName fields
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) []) -> generateGenericDefaultForRecords overrides typeName conName fields
    TyConI (DataD _ _ _ _ enums _) -> generateGenericDefaultForEnums overrides typeName enums
    TyConI (NewtypeD _ _ _ _ enums _) -> generateGenericDefaultForEnums overrides typeName [enums]
    TyConI (TySynD _n _ _enums) -> generateGenericDefaultForEnums overrides typeName []
    _ -> fail $ "can't generate GenericDefaults for type: " ++ show res
  -- runIO $ putStrLn (pprint result)
  pure result

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
    a@(_) -> do
      runIO $ putStrLn $ "non supported type: " ++ show a
      fail "Not Supported currently, to add support for your case check this: https://hackage.haskell.org/package/template-haskell-2.22.0.0/docs/Language-Haskell-TH.html#g:22"

generateGenericDefaultForRecords :: [(String, [String])] -> Name -> Name -> [VarBangType] -> Q [Dec]
generateGenericDefaultForRecords overrides typeName conName fields = do
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
          _ -> fail $ show typeName <> ", this shouldn't have happened, generated from previous step. something is wrong with the type :)"

  almostBody <- foldlM mkTypeValue (ConE conName) fieldCombinations
  let allDec = foldl' (\acc fieldCombination -> acc <> (snd fieldCombination)) [] fieldCombinations
  let allfieldDefaultsList = map fst fieldCombinations

  let body = DoE Nothing (allfieldDefaultsList ++ [NoBindS (AppE (VarE 'return) almostBody)])

  let finalBody = UInfixE (AppE (VarE 'take) (LitE (IntegerL 10))) (VarE '($)) body

  -- runIO $ putStrLn $ (pprint finalBody)

  let functionDec = [FunD 'genDef [Clause [VarP typeProxy] (NormalB finalBody) []]]
  let instanceDec = [InstanceD Nothing [] (AppT (ConT ''GenericDefaults) (ConT typeName)) functionDec]

  return $ DL.nub $ instanceDec <> allDec

generateGenericDefaultForEnums :: [(String, [String])] -> Name -> [Con] -> Q [Dec]
generateGenericDefaultForEnums _ _ [] = return []
generateGenericDefaultForEnums overrides enumName (firstEnum : _rest) = do
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
  let instanceDec = [InstanceD Nothing [] (AppT (ConT ''GenericDefaults) (ConT enumName)) functionDec]
  return $ DL.nub $ instanceDec <> innerDec
  where
    foldlF arrV acc fn = foldlM fn acc arrV
