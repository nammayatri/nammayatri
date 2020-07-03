{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Product.User.Get where

import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (withFlowHandler)
import Data.Aeson
import qualified Data.List as List
import Data.Time
import Epass.Product.Common
import qualified Epass.Storage.Queries.Location as Location
import qualified Epass.Storage.Queries.Organization as Org
import Epass.Types.API.Common
import Epass.Types.API.User
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Location as Location
import qualified Epass.Types.Storage.Organization as Org
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person
import Utils.Common

list ::
  RegToken ->
  Maybe Int ->
  Maybe Int ->
  Maybe LocateBy ->
  [Text] ->
  [Person.Role] ->
  FlowHandler ListRes
list regToken offsetM limitM locateM locate roleM = withFlowHandler $ do
  reg <- verifyToken regToken
  user <-
    fromMaybeM500 "Could not find user"
      =<< Person.findById (PersonId $ SR._EntityId reg)
  when (user ^. #_role == Person.DRIVER || user ^. #_role == Person.USER) $
    L.throwException $
      err400 {errBody = "UNAUTHORIZED_USER"}
  getUsers limitM offsetM locateM roleM locate user

getUsers ::
  Maybe Int ->
  Maybe Int ->
  Maybe LocateBy ->
  [Person.Role] ->
  [Text] ->
  Person.Person ->
  L.Flow ListRes
getUsers offsetM limitM locateM role locate user =
  case user ^. #_role of
    Person.ADMIN ->
      case locateM of
        Just LCITY -> cityLevelUsers limitM offsetM role locate
        Just LDISTRICT -> districtLevelUsers limitM offsetM role locate
        Just LWARD -> wardLevelUsers limitM offsetM role locate
        _ ->
          ListRes <$> Person.findAllWithLimitOffsetByRole limitM offsetM role
    Person.CITYLEVEL -> do
      org <- getOrgOrFail (user ^. #_organizationId)
      allLocations <-
        Location.findByStOrDistrict offsetM limitM LCITY (org ^. #_city)
      case locateM of
        Just LCITY ->
          if List.null locate || elem (org ^. #_city) locate
            then cityLevelUsers limitM offsetM role [org ^. #_city]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        Just LDISTRICT -> do
          let dists = mapMaybe Location._district allLocations
          let locateD =
                if List.null locate
                  then dists
                  else filter (`elem` dists) locate
          districtLevelUsers limitM offsetM role locateD
        Just LWARD -> do
          let wards = mapMaybe Location._ward allLocations
          let locateW =
                if List.null locate
                  then wards
                  else filter (`elem` wards) locate
          wardLevelUsers limitM offsetM role locateW
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    Person.DISTRICTLEVEL -> do
      org <- getOrgOrFail (user ^. #_organizationId)
      let district = fromJust $ org ^. #_district
      allLocations <-
        Location.findByStOrDistrict offsetM limitM LDISTRICT district
      case locateM of
        Just LDISTRICT ->
          if List.null locate || elem district locate
            then districtLevelUsers limitM offsetM role [district]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        Just LWARD -> do
          let wards = mapMaybe Location._ward allLocations
          let locateW =
                if List.null locate
                  then wards
                  else filter (`elem` wards) locate
          wardLevelUsers limitM offsetM role locateW
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    Person.WARDLEVEL -> do
      org <- getOrgOrFail (user ^. #_organizationId)
      let ward = fromJust $ org ^. #_ward
      case locateM of
        Just LWARD ->
          if List.null locate || List.elem ward locate
            then wardLevelUsers limitM offsetM role [ward]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}

getOrgOrFail :: Maybe Text -> L.Flow Org.Organization
getOrgOrFail orgId = do
  orgM <-
    join
      <$> mapM (Org.findOrganizationById . OrganizationId) orgId
  case orgM of
    Nothing -> L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOUND"}
    Just org -> return org

cityLevelUsers :: Maybe Int -> Maybe Int -> [Person.Role] -> [Text] -> L.Flow ListRes
cityLevelUsers limitM offsetM r cities =
  ListRes
    <$> ( Org.listOrganizations
            Nothing
            Nothing
            mempty
            mempty
            cities
            mempty
            mempty
            empty
            empty
            Nothing
            >>= Person.findAllWithLimitOffsetBy limitM offsetM r . map (^. #_id)
        )

districtLevelUsers ::
  Maybe Int -> Maybe Int -> [Person.Role] -> [Text] -> L.Flow ListRes
districtLevelUsers limitM offsetM r districts =
  ListRes
    <$> ( Org.listOrganizations
            Nothing
            Nothing
            mempty
            mempty
            mempty
            districts
            mempty
            empty
            empty
            Nothing
            >>= Person.findAllWithLimitOffsetBy limitM offsetM r . map (^. #_id)
        )

wardLevelUsers :: Maybe Int -> Maybe Int -> [Person.Role] -> [Text] -> L.Flow ListRes
wardLevelUsers limitM offsetM r wards =
  ListRes
    <$> ( Org.listOrganizations
            Nothing
            Nothing
            mempty
            mempty
            mempty
            mempty
            wards
            empty
            empty
            Nothing
            >>= Person.findAllWithLimitOffsetBy limitM offsetM r . map (^. #_id)
        )

get :: RegToken -> PersonId -> FlowHandler GetRes
get regToken userId = withFlowHandler $ do
  verifyToken regToken
  user <-
    fromMaybeM400 "User not found"
      =<< Person.findById userId
  locInfo <- mapM getLocationInfo (user ^. #_locationId)
  return $ mkUInfo user locInfo

listRoles :: RegToken -> FlowHandler [Person.Role]
listRoles regToken = withFlowHandler $ do
  verifyToken regToken
  pure $ enumFrom minBound
