{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module MicroLens where

import Prelude hiding (sum)
import Data.Monoid
import Control.Applicative
import qualified Data.Traversable as T

---------------------------------------------------------
-- Some basic libraries

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap ::  (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap ::  (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f

class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Profunctor (->) where
  dimap f g h = g . h . f

instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)

class Contravariant f where
  contramap :: (a' -> a) -> (f a -> f a')

-- Control.Applicative.Const replicated here for your
-- convenience
newtype K b a = K { getK :: b } deriving Functor

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K (e <> f)

instance Contravariant (K b) where
  contramap f (K b) = K b

newtype Id a = Id { getId :: a } deriving Functor

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

---------------------------------------------------------
-- The lens types you'll implement

-- | Optic is the general pattern for all other lens types.
type Optic p f s t a b =
  p a (f b) -> p s (f t)

type Iso s t a b =
  forall p f . (Profunctor p, Functor f) =>
  Optic p f s t a b

type Lens s t a b =
  forall f . Functor f => 
  Optic (->) f s t a b

type Traversal s t a b =
  forall f . Applicative f =>
  Optic (->) f s t a b

type Fold s a = 
  forall f . (Contravariant f, Applicative f) =>
  Optic (->) f s s a a

type Prism s t a b =
  forall p f . (Choice p, Applicative f) =>
  Optic p f s t a b

---------------------------------------------------------
---------------------------------------------------------
-- Todo

-- Lens :: (a -> f b) -> (s -> f t)
-- s t a b
-- (a -> f b) -> ((a, x) -> f (b, x))
-- | A lens focusing on the first element in a pair
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = fmap (\y -> (y, x)) (f a)

-- | A lens focusing on the second element in a pair
_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = fmap (\y -> (x, y)) (f a)

-- | A function which takes a lens and looks through it.
-- The type given is specialized to provide a hint as to
-- how to write 'view'. The more intuitive type for its use
-- is
--
-- @
-- view :: Lens s t a b -> (s -> a)
-- @
-- ((a -> K a b) -> (s -> K a t)) -> (s -> a)
view :: Optic (->) (K a) s t a b -> (s -> a)
view l = getK . l K

-- | A function which takes a lens and a transformation function
-- and applies that transformer at the focal point of the lens.
-- The type given is specialized to provide a hint as to how to
-- write 'over'. The more intuitive type for its use is
--
-- @
-- over :: Lens s t a b -> (a -> b) -> (s -> t)
-- @
over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over l f = getId . l (Id . f)

-- | A function from a lens and a value which sets the value
-- at the focal point of the lens. The type given has been
-- specialized to provide a hint as to how to write 'set'. The
-- more intuitive type for its use is
--
-- @
-- set :: Lens s t a b -> b -> (s -> t)
-- @
set :: Optic (->) Id s t a b -> b -> (s -> t)
set l x = getId . l (Id . const x)

-- | A traversal which focuses on each element in any 
-- Traversable container.
-- (Traversable f1, Applicative f2)
-- (a -> f2 b) -> ((f1 a) -> f2 (f1 b))
elements :: T.Traversable f => Traversal (f a) (f b) a b
elements f fa = T.traverse f fa

-- | A function which takes a Traversal and pulls out each 
-- element it focuses on in order. The type has been 
-- specialized, as the others, but a more normal type might be
--
-- @
-- toListOf :: Traversal s s a a -> (s -> [a])
-- @
toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf l s = appEndo (getK (l f s)) mempty
  where f = \a -> K $ (Endo (a :))

-- | A function which takes any kind of Optic which might
-- be focused on zero subparts and returns Just the first
-- subpart or else Nothing.
--
-- @
-- preview :: Traversal s s a a -> (s -> Maybe a)
-- @
preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview l s = getFirst $ getK (l f s)
  where f = \a -> K $ First (Just a)

-- | A helper function which witnesses the fact that any
-- container which is both a Functor and a Contravariant
-- must actually be empty.
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = contramap e . fmap e
  where e = const ()

-- | A Fold which views the result of a function application
-- (a -> b) -> ( (b -> f b) -> (a -> f a) )
to :: (a -> b) -> Fold a b
to f g = coerce . g . f

-- | A prism which focuses on the left branch of an Either
-- p a (f b) -> p (Either a x) (f (Either b x))
-- s = Either a x
-- t = Either b x
-- left' :: p a b -> p (Either a c) (Either b c)
-- p (Either a c) (Either (f b) c)
_Left :: Prism (Either a x) (Either b x) a b
_Left = rmap f . left'
  where f = either (fmap Left) (pure . Right)

-- | A prism which focuses on the right branch of an Either
_Right :: Prism (Either x a) (Either x b) a b
_Right = rmap f . right'
  where f = either (pure . Left) (fmap Right)

-- | An iso which witnesses that tuples can be flipped without
-- losing any information
_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip = dimap f (fmap f)
  where f (a, b) = (b, a)
