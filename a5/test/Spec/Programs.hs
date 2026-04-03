{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- |
module Spec.Programs
  ( plus
  , times
  , power
  , insertionSort
  , alwaysCrash
  ) where

import A5.Language
import A5.Language.Notation

-- | Imperative program for adding two numbers.
plus :: (Lang val cmp) => val -> val -> cmp
plus x y = do
  forUp 0 x y \_ acc -> do
    acc + 1

-- | Imperative program for adding two numbers.
times :: (Lang val cmp) => val -> val -> cmp
times x y = do
  forUp 0 x 0 \_ acc -> do
    acc + y

power :: (Lang val cmp) => val -> val -> cmp
power x y = do
  forUp 0 y 1 \_ acc -> do
    acc * x

-- | Imperative program for sorting a list.
insertionSort :: (Lang val cmp) => val -> cmp
insertionSort xs = do
  xlen <- lenArray xs
  forUp 1 xlen xs \i xs -> do
    iprev <- i - 1
    forDown iprev 0 xs \j xs -> do
      jsuc <- j + 1
      xj <- getArray xs j
      xjsuc <- getArray xs jsuc
      shouldSwap <- xj > xjsuc
      if shouldSwap then do
        xs' <- setArray xs j xjsuc
        setArray xs' jsuc xj
      else
        ret xs

-- | A program that will always crash, but requires a bit
-- of static analysis.
alwaysCrash :: (Lang val cmp) => val -> cmp
alwaysCrash x = do
  nx <- not x
  b <- x || nx
  if b then
    1 + true
  else
    ret 0
