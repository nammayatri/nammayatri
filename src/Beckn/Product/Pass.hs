module Beckn.Product.Pass where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Customer        as Customer
import qualified Beckn.Storage.Queries.CustomerDetail  as QCD
import qualified Beckn.Storage.Queries.Pass            as QP
import qualified Beckn.Storage.Queries.User            as User
import qualified Beckn.Storage.Queries.Organization    as Organization
import qualified Beckn.Storage.Queries.Document        as Document
import qualified Beckn.Storage.Queries.EntityDocument  as EntityDocument
import qualified Beckn.Storage.Queries.PassApplication as PassApplication
import qualified Beckn.Storage.Queries.EntityTag       as EntityTag
import qualified Beckn.Storage.Queries.Tag             as Tag
import qualified Beckn.Storage.Queries.Comment         as Comment
import           Beckn.Types.API.Pass
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..))
import qualified Beckn.Types.Storage.Customer          as Customer
import qualified Beckn.Types.Storage.CustomerDetail    as SCD
import           Beckn.Types.Storage.Pass
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Beckn.Types.Storage.PassApplication   as PassApplication
import qualified Beckn.Types.Storage.EntityDocument    as EntityDocument
import qualified Beckn.Types.Storage.Document          as Document
import qualified Beckn.Types.Storage.EntityTag         as EntityTag
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude                       hiding (pass)
import           Servant

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId =
  withFlowHandler $ do
    reg <- verifyToken regToken
    QP.findPassById passId >>=
      maybe
        (L.throwException $ err400 {errBody = "INVALID_DATA"})
        (\pass -> PassRes <$> getPassInfo pass)

getPassInfo :: Pass -> L.Flow PassInfo
getPassInfo Pass {..} = do
  morg <- maybe (pure Nothing) (Organization.findOrganizationById) $ _OrganizationId
  mcustomer <- maybe (pure Nothing) (Customer.findCustomerById) $ _CustomerId
  passApplication <- PassApplication.findById _PassApplicationId >>= (maybe (L.throwException $ err400 {errBody = "Pass Application not found"}) pure)
  entityDocs <- EntityDocument.findAllByPassApplicationId (PassApplication._id passApplication)
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> (traverse (Document.findById) (DocumentId <$> docIds))
  entityTags <- maybe (pure []) (\id-> EntityTag.findAllByEntity "PASS_APPLICATION" $ _getOrganizationId id) _OrganizationId
  let tagIds = EntityTag._TagId <$> entityTags
  tags <- catMaybes <$> (traverse (Tag.findById) (TagId <$> tagIds))
  comments <- maybe (pure []) (\id-> Comment.findAllByCommentedOnEntity "PASS_APPLICATION" $ _getOrganizationId id) _OrganizationId
  let toLocation = Location
                  { _type     = fromMaybe PINCODE _toLocationType
                  , _lat      = _toLat
                  , _long     = _toLong
                  , _ward     = _toWard
                  , _district = _toDistrict
                  , _city     = _toCity
                  , _state    = _toState
                  , _country  = _toCountry
                  , _pincode  = _toPincode
                  , _address  = _toAddress
                  , _bound   = _toBound
                  }
  let fromLocation = Location
          { _type     = fromMaybe PINCODE _fromLocationType
          , _lat      = _fromLat
          , _long     = _fromLong
          , _ward     = _fromWard
          , _district = _fromDistrict
          , _city     = _fromCity
          , _state    = _fromState
          , _country  = _fromCountry
          , _pincode  = _fromPincode
          , _address  = _fromAddress
          , _bound   = _fromBound
          }
  pure $ PassInfo
    { _fromLocation = fromLocation
    , _toLocation = toLocation
    , _Organization = morg
    , _Customer = mcustomer
    , _Comments = comments
    , _Tags = tags
    , _Documents = docs
    ,..
  }

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId UpdatePassReq{..} = withFlowHandler $ do
    RegistrationToken.RegistrationToken {..} <- verifyToken regToken
    pass <- fromMaybeM400 "Could not find Pass" =<< QP.findPassById passId
    pass' <-
      case _entityType of
        RegistrationToken.USER     -> do
          when (isNothing _action)
            (L.throwException $ err400 {errBody = "Status update cannot be empty"})

          when (isJust _CustomerId || isJust _fromLocation || isJust _toLocation)
            (L.throwException $ err400 {errBody = "Access denied"})

          return $ (pass { _status = fromJust _action } :: Pass)

        RegistrationToken.CUSTOMER -> do
          customer <- fromMaybeM500 "Could not find Customer" =<< Customer.findCustomerById (CustomerId _EntityId)
          case Customer._role customer of
            Customer.BUSINESSADMIN -> do
              when (isNothing _CustomerId && isNothing _toLocation)
                (L.throwException $ err400 {errBody = "Can update customerId or toLocation"})

              when (isJust _action || isJust _fromLocation)
                (L.throwException $ err400 {errBody = "Access denied"})

              return $
                case _toLocation of
                  Nothing           -> pass
                  Just location ->
                    pass  {
                        _toLocationType = Just (Location._type location)
                        , _toLat = (Location._lat location)
                        , _toLong = (Location._long location)
                        , _toWard = (Location._ward location)
                        , _toDistrict = (Location._district location)
                        , _toCity = (Location._city location)
                        , _toState = (Location._state location)
                        , _toCountry = (Location._country location)
                        , _toPincode = (Location._pincode location)
                        , _toAddress = (Location._address location)
                        , _toBound = (Location._bound location)
                      }

            Customer.INDIVIDUAL -> do
              when (isNothing _CustomerId && isNothing _fromLocation)
                (L.throwException $ err400 {errBody = "Can update customerId or fromLocation"})

              when (isJust _action || isJust _toLocation)
                (L.throwException $ err400 {errBody = "Access denied"})

              return $
                case _fromLocation of
                  Nothing           -> pass
                  Just location ->
                    pass  {
                        _fromLocationType = Just (Location._type location)
                        , _fromLat = (Location._lat location)
                        , _fromLong = (Location._long location)
                        , _fromWard = (Location._ward location)
                        , _fromDistrict = (Location._district location)
                        , _fromCity = (Location._city location)
                        , _fromState = (Location._state location)
                        , _fromCountry = (Location._country location)
                        , _fromPincode = (Location._pincode location)
                        , _fromAddress = (Location._address location)
                        , _fromBound = (Location._bound location)
                      }
    QP.updateMultiple passId pass'
    QP.findPassById passId
      >>= maybe (L.throwException $ err500 {errBody = "Could not find Pass"})
          (\pass -> PassRes <$> getPassInfo pass)

listPass ::
     Maybe Text
  -> PassIDType
  -> Text
  -> Maybe Int
  -> Maybe Int
  -> PassType
  -> FlowHandler ListPassRes
listPass regToken passIdType passV limitM offsetM passType =
  withFlowHandler $ do
    reg <- verifyToken regToken
    listBy <- getListBy
    passes <-  maybe (return []) getPasses listBy
    ListPassRes <$> traverse getPassInfo passes
  where
    getListBy =
      case passIdType of
        ORGANIZATIONID ->
          return $ Just $ QP.ByOrganizationId (OrganizationId passV)
        PASSAPPLICATIONID ->
          return $ Just $ QP.ByApplicationId (PassApplicationId passV)
        CUSTOMERID -> return $ Just $ QP.ByCustomerId (CustomerId passV)
        MOBILENUMBER -> do
          detail <- QCD.findByIdentifier SCD.MOBILENUMBER passV
          return $ (QP.ByCustomerId . SCD._CustomerId) <$> detail

    getPasses listBy =
      case (toEnum <$> limitM, toEnum <$> offsetM) of
        (Just l, Just o) -> QP.listAllPassesWithOffset l o listBy []
        _                -> QP.listAllPasses listBy []

