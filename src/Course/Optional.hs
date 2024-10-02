{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Optional where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Course.Core
import Prelude qualified as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a
  = Full a
  | Empty
  deriving (Eq, Show)

-- enum Optional<T> {
--   Full(T),
--   Empty,
-- }

-- | Return the possible value if it exists; otherwise, the first argument.
--
-- >>> fullOr 99 (Full 8)
-- 8
--
-- >>> fullOr 99 Empty
-- 99
fullOr ::
  a ->
  Optional a ->
  a
fullOr default' Empty = default'
fullOr default' (Full value) = value

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional ::
  (a -> b) ->
  Optional a ->
  Optional b
mapOptional f Empty = Empty
mapOptional f (Full value) = Full (f value)

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional ::
  (a -> Optional b) ->
  Optional a ->
  Optional b
bindOptional f Empty = Empty
bindOptional f (Full value) = f value

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) ::
  Optional a ->
  Optional a ->
  Optional a
(<+>) (Full value) right = Full value
(<+>) left (Full value) = Full value
(<+>) left right = Empty

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
optional ::
  (a -> b) ->
  b ->
  Optional a ->
  b
optional f default' Empty = default'
optional f default' (Full value) = f value
-- optional f default' opt = fullOr default' (mapOptional f opt)

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional f' a) f

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: (Eq a) => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
