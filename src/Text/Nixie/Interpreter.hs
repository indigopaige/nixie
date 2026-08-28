module Text.Nixie.Interpreter where

import Effectful.State.Static.Local
import Effectful.Error.Static
import Effectful

import qualified Text.Megaparsec as P
import Data.Void

import Control.Lens

import Text.Nixie.Ast
import Text.Nixie

import Data.List

