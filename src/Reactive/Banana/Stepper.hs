module Reactive.Banana.Stepper
  ( Stepper()
  , (-<@), (-->), accumS
  , changesS
  , behaviorS
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Prelude
import Reactive.Banana

-- | A @'Stepper' t a@ is a like a discrete behavior. It starts with some value, and
-- then later only changes whenever a given event fires. 
--
-- You can think of this type like @(a, 'Event' t a)@ but it has more useful instances
-- (for example, it has an 'Applicative' instance).
--
-- To construct values of this type, you can either use 'pure' or '(-->)'.
data Stepper t a = Stepper (Behavior t a) (Event t a)

instance Functor (Stepper t) where
  fmap f (Stepper i e) = Stepper (fmap f i) (fmap f e)

instance Applicative (Stepper t) where
  pure a = Stepper (pure a) never
  Stepper fB fEv <*> Stepper aB aEv = Stepper (fB <*> aB) $
    fmap (uncurry id) $ unionWith (\(f,_) (_,a) -> (f,a))
      (flip (,) <$> aB <@> fEv)
      ((,) <$> fB <@> aEv)

-- | @b -<\@ e@ constructs a new 'Stepper' that starts with the initial value of the
-- behavior b and samples the current value of @b@ whenever @e@ fires.
(-<@) :: Behavior t a -> Event t b -> Stepper t a
b -<@ e = Stepper b (b <@ e)
infixl 3 -<@

-- | @b --> e@ constructs a Stepper that starts with the initial value of behavior @b@
-- and after that always holds the value which the event @e@ had when it fired the
-- last time.
(-->) :: Behavior t a -> Event t a -> Stepper t a
b --> e = Stepper (fromMaybe <$> b <*> stepper Nothing (Just <$> e)) e

-- | Construct a new 'Stepper' by accumulating events.
accumS :: a -> Event t (a -> a) -> Stepper t a
accumS i e = pure i --> accumE i e

-- | Get an event that fires whenever the value of this stepper changes.
--
-- > changesS (pure a) = never
-- > changesS (b -<@ e) = (b <@ e)
changesS :: Stepper t a -> Event t a
changesS (Stepper _ e) = e

-- | Get a behavior that holds the current value of the stepper.
--
-- The value of the Behavior changes slightly after the changed event fired.
-- This allows for recursive definitions.
behaviorS :: Stepper t a -> Behavior t a
behaviorS (Stepper i _) = i
