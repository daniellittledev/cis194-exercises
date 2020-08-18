module Week01.CreditCardValidator
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  )
where

import Data.List

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) a b = b a

infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) a b x = a x |> b

rev :: [Integer] -> [Integer]
rev [] = []
rev (h:t) = rev t ++ [h]

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x | x < 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = rev . toDigits

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [l] = [l]
doubleEveryOther' (a:b:c) = [a, b * 2] ++ doubleEveryOther' c

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = rev . doubleEveryOther' . rev

sumDigits :: [Integer] -> Integer
sumDigits = map toDigits >>> concat >>> foldl (+) 0

validate :: Integer -> Bool
validate =
  toDigits
  >>> doubleEveryOther
  >>> sumDigits
  >>> (flip mod) 10
  >>> (==) 0
