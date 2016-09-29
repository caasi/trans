{-# LANGUAGE PackageImports #-}

module Main where

import Prelude ()
import B
import "ddd" D

main = putStrLn $ hello ++ ", " ++ world
