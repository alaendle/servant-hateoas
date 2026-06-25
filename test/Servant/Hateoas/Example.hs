{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.Example where

import           Data.Aeson      (ToJSON (..), Value (Array))
import           GHC.Generics
import           Servant
import Servant.Hateoas
    ( HasHandler(..),
      HasRelationLink(toRelationLink),
      Title,
      ToResource(..),
      Resource(wrap, addRel),
      MkLayers,
      resourcifyProxy,
      HasResourceServer(getResourceServer),
      Resourcify )
import Servant.Hateoas.ContentType.Collection (CollectionResource)
import Servant.Hateoas.ContentType.HAL 
import Data.Some.Constraint (Somes1(..))
import qualified Data.Vector as V

-- A reusable "key beside value" wrapper, à la persistent's Entity.
data With rel a = With { related :: rel, value :: a }

-- Serialize only the value; the relation key is internal.
instance ToJSON a => ToJSON (With rel a) where
  toJSON = toJSON . value

data UserRefs = UserRefs { selfId :: Int, addressRef :: Int, friends :: [Int], closeFriends :: [Int] }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON)

data User = User { name :: String, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON)

data Address = Address { street :: String, city :: String }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource res)

data EmbeddedList a = EmbeddedList [HALResource a]

instance ToJSON a => ToJSON (EmbeddedList a) where
  toJSON (EmbeddedList xs) = Array $ V.fromList $ toJSON <$> xs

instance ToResource HALResource (With UserRefs User) where
  toResource _ ct w@(With (UserRefs uid aid _ closeIds) _) = HALResource w [selfLink uid, addrLink aid, friendsLink uid, closeFriendsLink uid] [("close-friends", Some1 $ HALResource (EmbeddedList closeRes) [] [])]
    where
      closeUsers = filter (\(With (UserRefs u _ _ _) _) -> u `elem` closeIds) userDb
      closeRes = (\wl@(With (UserRefs uidl aidl _ _) _) -> HALResource wl [selfLink uidl, addrLink aidl, friendsLink uidl, closeFriendsLink uidl] []) <$> closeUsers
      addrLink         = ("address",) . toRelationLink (resourcifyProxy (Proxy @AddressGetOne) ct)
      selfLink         = ("self",) . toRelationLink (resourcifyProxy (Proxy @UserGetOne) ct)
      friendsLink      = ("friends",) . toRelationLink (resourcifyProxy (Proxy @UserGetFriends) ct)
      closeFriendsLink = ("close-friends",) . toRelationLink (resourcifyProxy (Proxy @UserGetCloseFriends) ct)

instance ToResource CollectionResource (With UserRefs User) where
  toResource _ ct w@(With (UserRefs uid aid _ _) _) = 
      addRel ("self", mkSelfLink uid)
    . addRel ("address", mkAddrLink aid)
    . addRel ("friends", mkFriendsLink uid)
    . addRel ("close-friends", mkCloseFriendsLink uid)
    $ wrap w
    where
      mkAddrLink         = toRelationLink $ resourcifyProxy (Proxy @AddressGetOne) ct
      mkSelfLink         = toRelationLink $ resourcifyProxy (Proxy @UserGetOne) ct
      mkFriendsLink      = toRelationLink $ resourcifyProxy (Proxy @UserGetFriends) ct
      mkCloseFriendsLink = toRelationLink $ resourcifyProxy (Proxy @UserGetCloseFriends) ct

type Api = UserApi :<|> AddressApi

type UserApi = UserGetOne :<|> UserGetAll :<|> UserGetQuery :<|> UserGetFriends :<|> UserGetCloseFriends
type UserGetOne     = "api" :> "user" :> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] (With UserRefs User)
type UserGetAll     = "api" :> "user" :> Get '[JSON] [With UserRefs User]
type UserGetQuery   = "api" :> "user" :> "query" :> QueryParam "name" String :> QueryParam "income" Double :>Get '[JSON] (With UserRefs User)
type UserGetFriends = "api" :> "user" :> Capture "id" Int :> "friends" :> Get '[JSON] [With UserRefs User]
type UserGetCloseFriends = "api" :> "user" :> Capture "id" Int :> "close-friends" :> Get '[JSON] [With UserRefs User]

type AddressApi = AddressGetOne
type AddressGetOne = "api" :> "address" :> Capture "id" Int :> Get '[JSON] Address

userDb :: [With UserRefs User]
userDb = [ With (UserRefs 1 1 [2, 3] [2]) (User "Alice" 1000)
         , With (UserRefs 2 2 [1, 3] [1]) (User "Bob" 2000)
         , With (UserRefs 3 2 [1, 2] [1]) (User "Charlie" 3000)
         ]

instance Monad m => HasHandler m UserGetOne where
  getHandler _ _ = \uId -> return $ case filter (\(With (UserRefs uid _ _ _) _) -> uid == uId) userDb of
    (user:_) -> user
    _        -> error "User not found"

instance Monad m => HasHandler m UserGetAll where
  getHandler _ _ = return userDb

instance Monad m => HasHandler m UserGetQuery where
  getHandler _ _ = \mName mIncome -> return $ case filter (\(With _ (User n i)) -> maybe True (== n) mName && maybe True (== i) mIncome) userDb of
      (user:_) -> user
      _        -> error "User not found"

instance Monad m => HasHandler m AddressGetOne where
  getHandler _ _ 1 = pure (Address "123 Main St" "Anytown")
  getHandler _ _ 2 = pure (Address "456 Elm St" "Othertown")
  getHandler _ _ _ = error "Address not found"

instance Monad m => HasHandler m UserGetFriends where
  getHandler _ _ = \uId -> return $ case filter (\(With (UserRefs uid _ _ _) _) -> uid == uId) userDb of
    (With (UserRefs _ _ friends _) _ : _) -> filter (\(With (UserRefs uid _ _ _) _) -> uid `elem` friends) userDb
    _                                     -> error "User not found"

instance Monad m => HasHandler m UserGetCloseFriends where
  getHandler _ _ = \uId -> return $ case filter (\(With (UserRefs uid _ _ _) _) -> uid == uId) userDb of
    (With (UserRefs _ _ _ closeFriends) _ : _) -> filter (\(With (UserRefs uid _ _ _) _) -> uid `elem` closeFriends) userDb
    _                                          -> error "User not found"

layerServer :: Server (Resourcify (MkLayers Api) (HAL JSON))
layerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers Api))

layerApp :: Application
layerApp = serve (Proxy @((Resourcify (MkLayers Api)) (HAL JSON))) layerServer

apiServer :: Server (Resourcify Api (HAL JSON))
apiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @Api)

apiApp :: Application
apiApp = serve (Proxy @((Resourcify Api) (HAL JSON))) apiServer
