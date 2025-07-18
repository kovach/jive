module Util where

import Text.Read ( readMaybe )

import Types
import Data.Function
import Data.List
import Data.Maybe

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

toSchema :: [(Pred, Arity)] -> Schema
toSchema m =
  let
    agg [] = []
    agg ((k,v) : xs) =
      let (matching, rest) = partition ((== k) . fst) xs
       in (k, v : map snd matching) : agg rest

    m' = agg m in
  \k -> case flip lookup m' k of
    Just v -> v
    Nothing -> error $ "undefined predicate: " <> k

parseDb :: String -> (Schema, [Tuple])
parseDb s =
    let (s1, s2) = split '.' s
        schema = toSchema $ parseSchema s1
        tuples1 = filter ((> 0) . length) (lines s2)
        db = map parseTuple tuples1
    in (schema, db)
  where

parseTuples = map parseTuple . lines

