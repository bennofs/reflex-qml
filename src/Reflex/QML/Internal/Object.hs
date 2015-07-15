{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Reflex.QML.Internal.Object where

import Control.Applicative
import Control.Monad.Writer
import Data.Semigroup.Applicative
import Graphics.QML.Objects
import Prelude
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.App
import Reflex.Host.Class

