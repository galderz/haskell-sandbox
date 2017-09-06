{-# LANGUAGE OverloadedStrings #-}

module PolymorphicParsing where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta
