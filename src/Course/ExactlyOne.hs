{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.ExactlyOne where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Course.Core
import Prelude qualified as P

name :: P.String
name = "Alex"

greeting :: P.String -> P.String
greeting name = "Hello, " ++ name
  where
    (++) = (P.++)

add :: Int -> Int -> Int
-- add a b = a + b
-- add a = (a+)
-- add x = \y -> x + y
add a = (+) a

-- add = (+)

functionOfAdding :: Int -> Int -> Int
-- functionOfAdding = add
-- functionOfAdding a b = add a b
functionOfAdding a b = a `add` b

data ExactlyOne a = ExactlyOne a
  deriving (Eq, Show)

runExactlyOne :: ExactlyOne a -> a
runExactlyOne (ExactlyOne a) = a

mapExactlyOne :: (a -> b) -> ExactlyOne a -> ExactlyOne b
mapExactlyOne f (ExactlyOne a) = ExactlyOne (f a)

bindExactlyOne :: (a -> ExactlyOne b) -> ExactlyOne a -> ExactlyOne b
bindExactlyOne f (ExactlyOne a) = f a

instance P.Functor ExactlyOne where
  fmap =
    M.liftM

instance A.Applicative ExactlyOne where
  (<*>) =
    M.ap
  pure =
    ExactlyOne

instance P.Monad ExactlyOne where
  (>>=) =
    flip bindExactlyOne
