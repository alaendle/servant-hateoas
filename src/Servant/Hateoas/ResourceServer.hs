{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.ResourceServer
(
  -- * Type-Class
  HasResourceServer(..),

  -- * Type-Families
  Resourcify,
  resourcifyProxy,
  ResourcifyServer
)
where

import Servant
import Servant.Hateoas.Layer
import Servant.Hateoas.Resource
import Servant.Hateoas.HasHandler
import Servant.Hateoas.RelationLink
import Servant.Hateoas.Internal.Polyvariadic
import Data.Kind

-- | Turns an API into a resourceful API by replacing the response type of each endpoint with a resource type.
type Resourcify :: k -> Type -> k
type family Resourcify api ct where
  Resourcify EmptyAPI        ct = EmptyAPI
  Resourcify (a :<|> b)      ct = Resourcify a ct :<|> Resourcify b ct
  Resourcify (a :> b)        ct = a :> Resourcify b ct
  Resourcify (Verb m s _ a)  ct = Verb m s '[ct] (MkResource ct (ResponsifyPayload ct a))
  Resourcify ('Layer api cs verb) ct = 'Layer (Resourcify api ct) (Resourcify cs ct) (Resourcify verb ct)
  Resourcify (x:xs)          ct = Resourcify x ct : Resourcify xs ct
  Resourcify a               _  = a

-- | A proxy function for 'Resourcify'.
resourcifyProxy :: forall api ct. Proxy api -> Proxy ct -> Proxy (Resourcify api ct)
resourcifyProxy _ _ = Proxy @(Resourcify api ct)

-- | Turns a 'ServerT' into a resourceful 'ServerT' by replacing the result type @m a@ of the function @server@ with @m (res a)@ where
-- @res := 'MkResource' ct@.
--
-- Together with 'Resourcify' the following 'Constraint' holds:
--
-- @
-- forall api ct m. ServerT (Resourcify api) ct m ~ ResourcifyServer (ServerT api m) ct m
-- @
type ResourcifyServer :: k -> Type -> (Type -> Type) -> Type
type family ResourcifyServer server ct m where
  ResourcifyServer EmptyServer ct m = EmptyServer
  ResourcifyServer (a :<|> b)  ct m = ResourcifyServer a ct m :<|> ResourcifyServer b ct m
  ResourcifyServer (a -> b)    ct m = a -> ResourcifyServer b ct m
  ResourcifyServer (m a)       ct m = m (MkResource ct (ResponsifyPayload ct a))
  ResourcifyServer (f a)       ct m = f (ResourcifyServer a ct m) -- needed for stepping into containers like [Foo]

-- | A typeclass providing a function to turn an API into a resourceful API.
class HasResourceServer api m ct where
  getResourceServer :: Monad m => Proxy m -> Proxy ct -> Proxy api -> ServerT (Resourcify api ct) m

instance {-# OVERLAPPING #-} (HasResourceServer a m ct, HasResourceServer b m ct) => HasResourceServer (a :<|> b) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @a) :<|> getResourceServer m ct (Proxy @b)

-- | A typeclass to automatically align the arity of the link generator with the server handler.
-- If the server expects an argument (like a Header) that the link generator drops, we pad it with `\_ ->`.
class AlignLink server link padded | server link -> padded where
  alignLink :: Proxy server -> link -> padded

instance {-# OVERLAPPABLE #-} (padded ~ link) => AlignLink server link padded where
  alignLink _ l = l

instance {-# OVERLAPS #-} AlignLink sB link pB => AlignLink (arg -> sB) link (arg -> pB) where
  alignLink _ l = \_ -> alignLink (Proxy @sB) l

instance {-# OVERLAPPING #-} AlignLink sB lB pB => AlignLink (arg -> sB) (arg -> lB) (arg -> pB) where
  alignLink _ l = \x -> alignLink (Proxy @sB) (l x)

-- | Adds a self-link to the resource.
instance {-# OVERLAPPABLE #-}
  ( server ~ ServerT api m
  , ServerT (Resourcify api ct) m ~ ResourcifyServer server ct m
  , mkLink ~ MkLink (Resourcify api ct) RelationLink
  , Accept ct
  , Resource (MkResource ct)
  , BuildResource ct a
  , HasHandler m api
  , HasRelationLink (Resourcify api ct)
  , AlignLink server mkLink paddedLink
  , PolyvariadicComp2 server paddedLink (IsFun server)
  , Return2 server paddedLink (IsFun server) ~ (m a, RelationLink)
  , Replace2 server paddedLink (m (MkResource ct (ResponsifyPayload ct a))) (IsFun server) ~ ResourcifyServer server ct m
  ) => HasResourceServer (api :: Type) m ct where
  getResourceServer m _ api = pcomp2 (\(ma, self) -> addSelfRel self . buildResource (Proxy @ct) <$> ma) (getHandler m api) paddedSelf
    where
      mkSelf = toRelationLink (Proxy @(Resourcify api ct))
      paddedSelf = alignLink (Proxy @server) mkSelf

instance
  ( api ~ LayerApi l
  , rApi ~ Resourcify api ct
  , ServerT (Resourcify l ct) m ~ ResourcifyServer (ServerT l m) ct m
  , rServer ~ ResourcifyServer (ServerT l m) ct m
  , res ~ MkResource ct
  , buildFun ~ ReplaceHandler rServer [(String, RelationLink)]
  , Resource res
  , BuildLayerLinks (Resourcify l ct) m
  , PolyvariadicComp buildFun (IsFun buildFun)
  , Return buildFun (IsFun buildFun) ~ [(String, RelationLink)]
  , Replace buildFun (m (res Intermediate)) (IsFun buildFun) ~ rServer
  ) => HasResourceServer l m ct where
  getResourceServer m _ _ = (return @m . foldr addRel (wrap @res $ Intermediate ())) ... buildLayerLinks (Proxy @(Resourcify l ct)) m

instance HasResourceServer ('[] :: [Layer]) m ct where
  getResourceServer _ _ _ = emptyServer

instance
  ( HasResourceServer ls m ct
  , HasResourceServer l m ct
  , BuildLayerLinks (Resourcify l ct) m
  ) => HasResourceServer (l ': ls) m ct where
  getResourceServer m ct _ = getResourceServer m ct (Proxy @l) :<|> getResourceServer m ct (Proxy @ls)
