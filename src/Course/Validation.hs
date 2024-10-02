{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Validation where

import qualified Prelude as P(String, all)
import Data.Char (isUpper)
import Course.Core

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap, either)
-- >>> instance Arbitrary a => Arbitrary (Validation a) where arbitrary = P.fmap (P.either Error Value) arbitrary
data Validation a = Error Err | Value a
  deriving (Eq, Show)

-- enum Validation<T> {
--   Value(T),
--   Error(Err),
-- }

type Err = P.String

greaterThanZero :: Validation Int -> Validation Int
greaterThanZero val = case val of
  Value x ->  if x > 0 then
                Value x
              else
                Error "Must be greater than zero"
  Error _ -> val

-- | Returns whether or not the given validation is an error.
--
-- >>> isError (Error "message")
-- True
--
-- >>> isError (Value 7)
-- False
--
-- prop> \x -> isError x /= isValue x
isError :: Validation a -> Bool
isError (Error _) = True
isError (Value _) = False

-- | Returns whether or not the given validation is a value.
--
-- >>> isValue (Error "message")
-- False
--
-- >>> isValue (Value 7)
-- True
--
-- prop> \x -> isValue x /= isError x
isValue :: Validation a -> Bool
isValue = not . isError

-- | Maps a function on a validation's value side.
--
-- >>> mapValidation (+10) (Error "message")
-- Error "message"
--
-- >>> mapValidation (+10) (Value 7)
-- Value 17
--
-- prop> \x -> mapValidation id x == x
mapValidation :: (a -> b) -> Validation a -> Validation b
mapValidation _ (Error s) = Error s
mapValidation f (Value a) = Value (f a)

-- | Binds a function on a validation's value side to a new validation.
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Error "message")
-- Error "message"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 7)
-- Error "odd"
--
-- >>> bindValidation (\n -> if even n then Value (n + 10) else Error "odd") (Value 8)
-- Value 18
--
-- prop> \x -> bindValidation Value x == x
bindValidation :: (a -> Validation b) -> Validation a -> Validation b
bindValidation _ (Error s) = Error s
bindValidation f (Value a) = f a

(>>==) :: Validation a -> (a -> Validation b) -> Validation b
(>>==) = flip bindValidation

-- defp lookup(mail_class, method, size, zone) when mail_class in @zoneless_mail_classes do
--     # I was going to call it the Nozone Layer, but I decided against it.
--     with {:ok, rating_methods} <- Map.fetch(@zoneless_rates, mail_class),
--          {:ok, rates} <- Map.fetch(rating_methods, method) do
--       lookup_zoneless_rate(rates, size, zone)
--     else
--       _ -> nil
--     end
--   end

data RatingMethod = MkRatingMethod P.String deriving Show
data Rate = MkRate P.String deriving Show

requireUpperCase :: P.String -> Validation P.String
requireUpperCase s = if P.all isUpper s then
                       Value s
                     else
                        Error "Must be upper case"

ratingMethod :: P.String -> Validation RatingMethod
ratingMethod s = mapValidation MkRatingMethod ratingMethodName
  where ratingMethodName :: Validation P.String
        ratingMethodName = Value s >>== requireUpperCase

lookup :: RatingMethod -> Validation [Rate]
lookup (MkRatingMethod "S") = Value [MkRate "S0", MkRate "S1", MkRate "S2"]
lookup (MkRatingMethod "T") = Value []
lookup _ = Error "Unknown rating method"

listFirst :: [a] -> Validation a
listFirst [] = Error "No values"
listFirst (x:_) = Value x

doTheThing :: P.String -> Validation Rate
doTheThing s = Value s >>== ratingMethod >>== lookup >>== listFirst

-- | Returns a validation's value side or the given default if it is an error.
--
-- >>> valueOr (Error "message") 3
-- 3
--
-- >>> valueOr (Value 7) 3
-- 7
--
-- prop> \x -> isValue x || valueOr x n == n
valueOr :: Validation a -> a -> a
valueOr (Error _) a = a
valueOr (Value a) _ = a

-- | Returns a validation's error side or the given default if it is a value.
--
-- >>> errorOr (Error "message") "q"
-- "message"
--
-- >>> errorOr (Value 7) "q"
-- "q"
--
-- prop> \x -> isError x || errorOr x e == e
errorOr :: Validation a -> Err -> Err
errorOr (Error e) _ = e
errorOr (Value _) a = a

valueValidation :: a -> Validation a
valueValidation = Value
