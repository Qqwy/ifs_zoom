module Pipe ((|>)) where

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>
{-# INLINE (|>) #-}

