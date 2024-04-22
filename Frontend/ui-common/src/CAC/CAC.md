Why and What are needed in CAC.
    1. This is Unify the configs through out the apps.
    2. This config consist of configs which are specific to particular context for each app.
        Ex. 
        1. For NammaYatri Driver, We need to disable Subscription for Chennai we can add it to CAC.
        2. For Hyderabad, We need to disable Support in NammaYatri App but For ManaYatri, We might enable Support. It will be handled in single place.
    3. We will add only things which will change based on the particular context param.  For now we are focusing on City, Merchant.

What are not required in CAC.
    1. Ui customization based on merchant.
    2. Config specific to merchant only.

What will be added in future.
    1. Currently its going to be a case match and Hash Map.
    2. we will move this to a local DB for faster retrival.
    3. Once we move CAC to backend we will have this as a fallback.  

```haskell
getFromCAC :: Maybe City -> Maybe Merchant -> CACConfig
getFromCAC mbCity mbMerchant = do
    shouldRenewConfig <- isContextChanged $ getContextKey (fromMaybe AnyCity mbCity) mbMerchant
    if shouldRenewConfig 
        then fetchFromCAC mbCity mbMerchant
        else getExistingCAC


fetchFromCAC :: Maybe String -> Maybe Merchant -> CACConfig
fetchFromCAC mbCity mbMerchant = 
    case mbCity of
        Nothing -> fetchCACByMerchant AnyCity merchant
        Just city -> fetchCACByMerchant city merchant

fetchCACByMerchant :: City -> Maybe Merchant -> CACConfig
fetchCACByMerchant city mbMerchant = 
    let config = case city of
                    AnyCity -> { enableSubscription : false }
                    Bangalore  -> { enableSubscription : case mbMerchant of 
                            Just NammaYatri -> true
                            _ -> false}
                    _ -> { enableSubscription : false }
    storeConfigWithContextInCache config getContextKey

getContextKey city mbMerchant = show city <> 
            case mbMerchant of
                Just merchant -> "_" <> merchant
                Nothing -> ""

isContextChanged :: String -> Boolean
isContextChanged key = if key == (currentContext "") then false else true 


foreign import currentContext :: String -> String
```