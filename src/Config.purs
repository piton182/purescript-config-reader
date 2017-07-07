module Config where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Reader.Class (class MonadAsk)
import RxJS.Observable (Observable, timer, subscribeNext)

newtype ConfigReader m a = ConfigReader (m a)

instance functorConfigReader :: Functor f => Functor (ConfigReader f) where
  map g (ConfigReader f) = ConfigReader $ (g <$> f)

instance applyConfigReader :: Apply f => Apply (ConfigReader f) where
  apply (ConfigReader fg) (ConfigReader fx) = ConfigReader (apply fg fx)

instance applicativeConfigReader :: Applicative f => Applicative (ConfigReader f) where
  pure = ConfigReader <<< pure

instance bindSubscripton :: Bind m => Bind (ConfigReader m) where
  bind (ConfigReader m) g = ConfigReader $ bind m \x ->
    case g x of
      (ConfigReader m') -> m'

instance monadConfigReader :: Monad m => Monad (ConfigReader m)

class RunConfigReader m where
  runConfigReader :: forall cfg eff. m cfg -> (cfg -> Eff eff Unit) -> Eff eff Unit

--

newtype Latest a = Latest a

instance showLatest :: Show a => Show (Latest a) where
  show (Latest x) = show x

newtype Timer = Timer Int

instance showTimer :: Show Timer where
  show (Timer t) = show t

instance runConfigReaderObservableSubscription :: RunConfigReader (ConfigReader Observable) where
  runConfigReader (ConfigReader cfgs) g = void $ subscribeNext g cfgs

instance monadAskConfigReader :: MonadAsk (Latest Timer) (ConfigReader Observable) where
  ask = ConfigReader $ do
    n <- timer 3000 3000
    pure $ Latest (Timer n)
