> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE RankNTypes #-}
> import Data.Text (Text, pack, unpack)

Lens 

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) . (.) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.) . (.) . (.)
  :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c

fmap :: Functor f => (a -> b) -> f a -> f b
fmap.fmap
  :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap.fmap.fmap
  :: (Functor f1, Functor f2, Functor f3) =>
     (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))

> type SEC s t a b = (a -> b) -> s -> t

> result :: SEC (c -> a) (c -> b) a b
> result = (.)
> element :: SEC [a] [b] a b
> element = fmap
> second :: SEC (c, a) (c, b) a b
> second = fmap
> first :: SEC (a, c) (b, c) a b
> first f (a,b) = (f a, b)
> fmap' :: Functor f => SEC (f a) (f b) a b
> fmap' = fmap

Setters
traverse
  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse.traverse
  :: (Applicative f, Traversable t1, Traversable t2) =>
     (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b))
traverse.traverse.traverse
  :: (Applicative f, Traversable t1, Traversable t2,
      Traversable t3) =>
     (a -> f b) -> t1 (t2 (t3 a)) -> f (t1 (t2 (t3 b)))

class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

> fmapDefault :: Traversable t => (a -> b) -> t a -> t b
> fmapDefault f = runIdentity . traverse (Identity . f)

> newtype Identity a = Identity { runIdentity :: a }
> instance Functor Identity where
>   fmap f (Identity a) = Identity (f a)
> instance Applicative Identity where
>   pure = Identity
>   Identity f <*> Identity a = Identity (f a)
> over
>   :: Setter s t a b
>      -> (a -> b) -> s -> t
> over l f = runIdentity . l (Identity . f)

> type Setter s t a b = (a -> Identity b) -> s -> Identity t

over traverse f = runIdentity . traverse (Identity . f)
                = fmapDefault f
                = fmap f

> mapped :: Functor f => Setter (f a) (f b) a b
> mapped f = Identity . fmap (runIdentity . f)

over mapped f = runIdentity . mapped (Identity . f)
              = runIdentity . Identity . fmap (runIdentity . Identity . f)
              = fmap f

over mapped (+1) [1,2,3] = [2,3,4]
over (mapped.mapped) (+1) [[1,2],[3]] = [[2,3],[4]]
over (mapped.mapped) length [["hello","world"],["!!!"]] = [[5,5],[3]]

> chars :: (Char -> Identity Char) -> Text -> Identity Text
> chars f = fmap pack . mapped f . unpack

over chars :: (Char -> Char) -> Text -> Text
over (mapped.chars)
  :: Functor f => (Char -> Char) -> f Text -> f Text
over (traverse.chars)
  :: Traversable t => (Char -> Char) -> t Text -> t Text

Functor Laws: 
fmap id = id
fmap f . fmap g = fmap (f . g)

Setter Laws for Setter l
over l id = id
over l f . over l g = over l (f . g)

> -- both :: Setter (a, a) (b, b) a b
> both :: Traversal (a, a) (b, b) a b
> both f (a,b) = (,) <$> f a <*> f b

> first' :: Lens (a, c) (b, c) a b
> first' f (a,c) = (,c) <$> f a

type Traversal1 s t a b = forall f. Apply f => (a -> f b) -> s -> f t
type Fold1 s a = forall f. (Contravariant f, Apply f) => (a -> f a) -> s -> f s
type Traversal1' s a = Traversal1 s s a a = forall f. Apply f => (a -> f a) -> s -> f s

over (mapped.both) length [("hello", "world"), ("!!", "!!!")] = [(5,5),(2,3)]
over both (+1) (2,3) = (3,4)

> type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

traverse :: Traversable f => Traversal (f a) (f b) a b
traverse.traverse :: (Traversable f, Traversable g) => Traversal (f(g a)) (f(g b)) a b

over traverse f = runIdentity . traverse (Identity . f)
                = fmapDefault f
                = fmap f

> newtype WrappedMonad m a
>   = WrapMonad {unwrapMonad :: m a}
> instance Monad m => Functor (WrappedMonad m) where
>   fmap f = WrapMonad . fmap f . unwrapMonad
> instance Monad m => Applicative (WrappedMonad m) where
>   pure = WrapMonad . pure
>   WrapMonad f <*> WrapMonad a = WrapMonad (f <*> a)

> mapM :: (Traversable f, Monad m) => (a -> m b) -> f a -> m (f b)
> mapM f = unwrapMonad . traverse (WrapMonad . f) 
> -- mapM f = traverse f 


> mapMOf :: Monad m => Traversal s t a b -> (a -> m b) -> s -> m t
> mapMOf l f = unwrapMonad . l (WrapMonad . f)
> -- mapMOf l f = l f

mapMOf traverse f = unwrapMonad . traverse (WrapMonad . f)
                  = mapM f

Traversable Laws:
traverse pure = pure
Compose . fmap (traverse f) . traverse g = traverse (Compose . fmap f . g)

Traversal laws for a valid traversal l
l pure = pure
Compose . fmap (l f) . l g = l (Compose . fmap f . g)


(2,3) & _2 .~ "hello" = (2,"hello")
_2 .~ "hello" $ (2,3) = (2,"hello")
(2,3) & _2 .~ "hello" & _1 .~ "world" = ("world","hello")
_1 .~ "world" $ _2 .~ "hello" $ (2,3) =  ("world","hello")


foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap.foldMap
  :: (Monoid m, Foldable t1, Foldable t2) =>
     (a -> m) -> t1 (t2 a) -> m
foldMap.foldMap.foldMap
  :: (Monoid m, Foldable t1, Foldable t2, Foldable t3) =>
     (a -> m) -> t1 (t2 (t3 a)) -> m

> newtype Const m a = Const { getConst :: m }
> instance Functor (Const m) where
>   fmap _ (Const m) = Const m
> instance Monoid m => Applicative (Const m) where
>   pure _ = Const mempty
>   Const m <*> Const n = Const (m <> n)

> foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
> foldMapDefault f = getConst . traverse (Const . f) 

> type Fold s a = forall m. Monoid m => (a -> Const m a) -> s -> Const m s
> foldMapOf :: Getting m s a -> (a -> m) -> s -> m
> foldMapOf l f = getConst . l (Const . f)

> folded :: (Foldable f) => Fold (f a) a
> folded f = Const . foldMap (getConst . f) 

--
folded.folded :: (Foldable f, Foldable g, Monoid m) => Fold (f (g a)) a
folded.folded.folded :: (Foldable f, Foldable g, Foldable h, Monoid m) => Fold (f (g (h a))) a

foldMapOf folded f = getConst . folded (Const . f)
                   = getConst . Const . foldMap (getConst . Const . f)
                   = foldMap f

> view :: Getting a s a -> s -> a
> view l = foldMapOf l id

--
--view folded = getConst . folded Const
--            = getConst . Const . foldMap (getConst . Const)
--            = foldMap id
--            = fold
--

> data Any = Any { getAny :: Bool }
> instance Semigroup Any where
>   Any l <> Any r = Any (l || r)
> instance Monoid Any where
>   mempty = Any False
> data Sum i = Sum { getSum :: i }
> instance Num i => Semigroup (Sum i) where
>   Sum l <> Sum r = Sum (l + r)
> instance Num i => Monoid (Sum i) where
>   mempty = Sum 0

> anyOf :: Fold s a -> (a -> Bool) -> s -> Bool
> anyOf l f = getAny . foldMapOf l (Any . f)
> sumOf :: Num i => Fold s i -> s -> i
> sumOf l = getSum . foldMapOf l Sum

anyOf folded (==2) [1] = False

((a -> Const a b) -> s -> Const a t) -> s -> a

> type Getting r s a = (a -> Const r a) -> s -> Const r s
> type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

> toListOf :: Getting [a] s a -> s -> [a]
> toListOf l = foldMapOf l pure

> type Getter s a = forall r. (a -> Const r a) -> s -> Const r s
> to :: (s -> a) -> Getter s a
> to f g = Const . getConst . g . f
> set :: Setter s t a b -> b -> s -> t
> set l b = over l (const b)
> lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
> lens sa bst afb s = (bst s) <$> afb (sa s)


 type Getting r s a = (a -> Const r a) -> s -> Const r s
 type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
 type Setter s t a b = (a -> Identity b) -> s -> Identity t
 type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
 type Fold s a = forall m. Monoid m => (a -> Const m a) -> s -> Const m s
