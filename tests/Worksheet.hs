--
-- Experiment with retrofit of "error" based, unsafe code with well-behaved, total behavior.
-- What's the pattern that starts with a base of Either and migrates to the Monad Transformer EitherT?
--

import Data.List

-- This one's a bad example because it's well behaved because drop and take are well-behaved!
rotate :: Int -> [a] -> [a]
rotate x xs =
  drop x' xs ++ take x' xs
  where
    x' = x `mod` length xs

-- This one is poorly behaved because it can call error.
rotateTo :: (Ord a, Show a) => a -> [a] -> [a]
rotateTo x xs =
  case elemIndex x xs of
    Nothing -> error $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    Just i  -> rotate i xs

-- Mapping this to plain-old Either is trivial:
tryRotateTo :: (Show a, Eq a) => a -> [a] -> Either String [a]
tryRotateTo x xs =
  case elemIndex x xs of
    Nothing -> Left $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    Just i  -> Right $ rotate i xs

-- Is this better?
tryRotateTo' :: (Show a, Eq a) => a -> [a] -> Either String [a]
tryRotateTo' x xs =
  maybe (Left $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs) (Right . flip rotate xs) (elemIndex x xs)


