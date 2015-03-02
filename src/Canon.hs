
-- | Canon API.
--   Records SimpleCanon, ScalesCanon, Canon embodying musical
--   canon type with increasing levels of parameterization.
--   Functions simpleCanonToScore, scalesCanonToScore, 
--   canonToScore answer Score for different Canon types.
module Canon (
  SimpleCanon(..)
  , simpleCanonToScore
  , ScalesCanon(..)
  , scalesCanonToScore
  , Canon(..)
  , canonToScore
  ) where

import Canon.Data
import Canon.Utils
