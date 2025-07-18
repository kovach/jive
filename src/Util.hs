module Util where

import Text.Read ( readMaybe )

import Types
import Data.Function

split delim s = (takeWhile (/= delim) s, drop 1 $ dropWhile (/= delim) s)

parseLit s =
  case readMaybe s of
    Just i -> LitInt i
    _ -> LitSym s

parseTuple s =
  case words s of
  (p : ts) -> Tuple p (map parseLit ts)
  _ -> error "attempt to parseTuple of empty string"

parseSchema s =
  words s
  & map (split '/')
  & map (\(a,b) -> (a, read b))

parseDb s =
  let (s1, s2) = split '.' s in
  let schema = parseSchema s1 in
  let tuples1 = filter ((> 0) . length) (lines s2)
  in (schema, map parseTuple tuples1)

parseTuples = map parseTuple . lines

