module Main where

import Prelude hiding (Word, lex)
import Parse

main = do
  putStrLn "example parses:"
  chk1
  putStrLn "example queries:"
  chk "cat saw X with Z"
  chk "(friend X) on Y"

{-

# Notes

# Todo
  repl
  articles
-}

