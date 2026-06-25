{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Servant.Hateoas.ExampleSpec (spec) where

import           Data.Type.Equality                     ((:~:) (Refl))
import           Servant
import           Servant.Hateoas                        (GoLayers, Intermediate,
                                                         Layer (..),
                                                         MergeLayers, Normalize,
                                                         Title,
                                                         getResourceServer)
import           Servant.Hateoas.ContentType.Collection (Collection)
import           Servant.Hateoas.Example                (Address, Api, User,
                                                         UserGetAll, apiApp,
                                                         layerApp)
import           Servant.Hateoas.Internal.Sym           (Sym, Symify)
import           Servant.Hateoas.ResourceServer         (Resourcify)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher                 (bodyEquals)

collectionServer :: Server (Resourcify UserGetAll (Collection JSON))
collectionServer = getResourceServer (Proxy @Handler) (Proxy @(Collection JSON)) (Proxy @UserGetAll)

collectionApp :: Application
collectionApp = serve (Proxy @(Resourcify UserGetAll (Collection JSON))) collectionServer

spec :: Spec
spec = do
  with (pure apiApp) $ do
    describe "HAL API" $
      describe "UserGetOne (GET /api/user/:id)" $
        it "returns the requested user as a HAL resource" $
          get "/api/user/1" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/0\",\"type\":\"application/hal+json\"},\"friends\":{\"href\":\"/api/user/1/friends\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/1\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":0,\"friends\":[],\"income\":0,\"usrId\":1}" }
    describe "Collection API" $
      describe "UserGetAll (GET /api/user)" $
        it "returns all users as a HAL collection" $
          get "/api/user" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{\"items\":[{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/1\",\"type\":\"application/hal+json\"},\"friends\":{\"href\":\"/api/user/1/friends\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/1\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":1,\"friends\":[2,3],\"income\":1000,\"usrId\":1},{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/2\",\"type\":\"application/hal+json\"},\"friends\":{\"href\":\"/api/user/2/friends\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/2\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":2,\"friends\":[],\"income\":2000,\"usrId\":2},{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address/3\",\"type\":\"application/hal+json\"},\"friends\":{\"href\":\"/api/user/3/friends\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/3\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}},\"addressId\":3,\"friends\":[],\"income\":3000,\"usrId\":3}]},\"_links\":{\"self\":{\"href\":\"/api/user\",\"type\":\"application/hal+json\"}}}" }
  with (pure collectionApp) $
    describe "Collection API" $
      describe "UserGetAll (GET /api/user)" $
        it "returns all users as a HAL collection" $
          get "/api/user" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"collection\":{\"items\":[{\"data\":[{\"name\":\"usrId\",\"value\":3},{\"name\":\"income\",\"value\":3000},{\"name\":\"friends\",\"value\":[]},{\"name\":\"addressId\",\"value\":3}],\"links\":[{\"href\":\"/api/user/3/friends\",\"rel\":\"friends\"},{\"href\":\"/api/address/3\",\"rel\":\"address\"},{\"href\":\"/api/user/3\",\"rel\":\"self\"}]},{\"data\":[{\"name\":\"usrId\",\"value\":2},{\"name\":\"income\",\"value\":2000},{\"name\":\"friends\",\"value\":[]},{\"name\":\"addressId\",\"value\":2}],\"links\":[{\"href\":\"/api/user/2/friends\",\"rel\":\"friends\"},{\"href\":\"/api/address/2\",\"rel\":\"address\"},{\"href\":\"/api/user/2\",\"rel\":\"self\"}]},{\"data\":[{\"name\":\"usrId\",\"value\":1},{\"name\":\"income\",\"value\":1000},{\"name\":\"friends\",\"value\":[2,3]},{\"name\":\"addressId\",\"value\":1}],\"links\":[{\"href\":\"/api/user/1/friends\",\"rel\":\"friends\"},{\"href\":\"/api/address/1\",\"rel\":\"address\"},{\"href\":\"/api/user/1\",\"rel\":\"self\"}]}],\"links\":[{\"href\":\"/api/user\",\"rel\":\"self\"}],\"version\":\"1.0\"}}" }
  with (pure layerApp) $
    describe "Test Layer API" $ do
      it "returns the api layer" $
        get "/api" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"address\":{\"href\":\"/api/address\",\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api\",\"type\":\"application/hal+json\"},\"user\":{\"href\":\"/api/user\",\"type\":\"application/hal+json\"}}}" }
      it "returns the user layer" $
        get "/api/user" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"id\":{\"href\":\"/api/user/{id}\",\"templated\":true,\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user\",\"title\":\"The user with the given id\",\"type\":\"application/hal+json\"}}}" }
      it "returns the query operation" $
        get "/api/user/query" `shouldRespondWith` 200 { matchBody = bodyEquals "{\"_embedded\":{},\"_links\":{\"addrId\":{\"href\":\"/api/user/query{?addrId}\",\"templated\":true,\"type\":\"application/hal+json\"},\"income\":{\"href\":\"/api/user/query{?income}\",\"templated\":true,\"type\":\"application/hal+json\"},\"self\":{\"href\":\"/api/user/query\",\"type\":\"application/hal+json\"}}}" }

-- This is intentionally unused, but it is a compile-time test that the Normalize type family works as expected.
checkNormalize :: Normalize Api :~:
  ( "api"
    :> ( "user"
      :> ( "query" :> QueryParam "addrId" Int :> QueryParam "income" Double :> Get '[JSON] User
        :<|> (Get '[JSON] [User]
        :<|> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] User
        :<|> Capture "id" Int :> "friends" :> Get '[JSON] [User]))
    :<|> "address" :> Capture "id" Int :> Get '[JSON] Address)
  )
checkNormalize = Refl

-- This is intentionally unused, but it is a compile-time test that the Normalize/Symify type family works as expected.
checkNormalizeSymify :: Normalize (Symify Api) :~:
  ( Sym "api"
    :> ( Sym "user"
      :> ( Sym "query" :> QueryParam "addrId" Int :> QueryParam "income" Double :> Get '[JSON] User
        :<|> (Get '[JSON] [User]
        :<|> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] User
        :<|> Capture "id" Int :> Sym "friends" :> Get '[JSON] [User]))
    :<|> Sym "address" :> Capture "id" Int :> Get '[JSON] Address)
  )
checkNormalizeSymify = Refl

-- This is intentionally unused, but it is a compile-time test that the Normalize/Symify type family works as expected.
checkGoLayers :: GoLayers (Normalize (Symify Api)) '[] :~:
  '[ 'Layer '[]                                        '[Sym "api"] (Get '[] Intermediate),
     'Layer '[Sym "api"]                               '[Sym "user"] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user"]                   '[Sym "query"] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Sym "query"]      '[QueryParam "addrId" Int] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Sym "query"]      '[QueryParam "income" Double] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Title "The user with the given id"] '[Capture "id" Int] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user"]                   '[Capture "id" Int] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Capture "id" Int] '[Sym "friends"] (Get '[] Intermediate),
     'Layer '[Sym "api"]                               '[Sym "address"] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "address"]                '[Capture "id" Int] (Get '[] Intermediate)]
checkGoLayers = Refl

-- This is intentionally unused, but it is a compile-time test that the Normalize/Symify type family works as expected.
checkMergeLayers :: MergeLayers (GoLayers (Normalize (Symify Api)) '[]) '[] :~:
  '[ 'Layer '[Sym "api", Sym "address"]                '[Capture "id" Int] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Capture "id" Int] '[Sym "friends"] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Title "The user with the given id"] '[Capture "id" Int] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user", Sym "query"]      '[QueryParam "addrId" Int, QueryParam "income" Double] (Get '[] Intermediate),
     'Layer '[Sym "api", Sym "user"]                   '[Sym "query", Capture "id" Int] (Get '[] Intermediate),
     'Layer '[Sym "api"]                               '[Sym "user", Sym "address"] (Get '[] Intermediate),
     'Layer '[]                                        '[Sym "api"] (Get '[] Intermediate)]
checkMergeLayers = Refl
