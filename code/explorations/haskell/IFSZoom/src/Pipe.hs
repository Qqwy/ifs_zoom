{-|
 Module      : Pipe
 Copyright   : [2020] Wiebe-Marten Wijnja
 License     : BSD3

 Maintainer  : Wiebe-Marten Wijnja <w-m@wmcode.nl>
 Stability   : stable
 Portability : portable

A one-function module that exposes a left-to-right function application operator.
 -}

module Pipe ((|>)) where


-- | left-to-right application of a function to a value.
-- ```
-- (x |> f) = (f x)
-- ```
--
-- This is similar to Data.Function.& but more readable.
-- It is based on the same syntax that exists in e.g. F#, Elm, Elixir.
(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>
{-# INLINE (|>) #-}

