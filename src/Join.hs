module Join where

import Data.Function
import Data.Maybe
import Data.Text ( splitOn )
import Types
import Control.Monad (foldM)
import Text.Read

data Pattern = Pattern Pred [Term]
  deriving Show
data Tuple = Tuple Pred [Literal]
instance Show Tuple where
  show (Tuple p ts) = p <> " " <> unwords (map show ts)
type Binding = [(Var, Literal)]

extend subst v l = (v,l) : subst

toPattern (Atom p ts) = Pattern p ts

unifyPattern :: Pattern -> [Tuple] -> [Binding]
unifyPattern (Pattern p ts) tuples = mapMaybe ok tuples
  where
    n = length ts
    ok (Tuple p' vs) =
      if p == p' && length vs == n then foldM unify [] (zip ts vs) else Nothing
    unify subst (TermVar v, l) = Just (extend subst v l)
    unify subst (TermLit l', l) = if l' == l then Just subst else Nothing

bjoin :: Binding -> Binding -> Maybe Binding
bjoin b1 b2 = foldM step b1 b2
  where
    step subst (k,v) =
      case lookup k subst of
        Nothing -> pure $ (k,v) : subst
        Just v' -> if v == v' then pure subst else Nothing

bjoins as bs = flip concatMap as (\a -> mapMaybe (bjoin a) bs)

type DB = [Tuple]
joins :: [Pattern] -> DB -> [Binding] -> [Binding]
joins [] _ bs = bs
joins (p : ps) db bs = joins ps db $ bjoins (unifyPattern p db) bs

parseLit s =
  case readMaybe s of
    Just i -> LitInt i
    _ -> LitSym s
parseTuple s =
  let (p : ts) = words s
  in Tuple p (map parseLit ts)

split delim s = (takeWhile (/= delim) s, drop 1 $ dropWhile (/= delim) s)

parseSchema s =
  words s
  & map (split '/')
  & map (\(a,b) -> (a, read b))

parseDb s =
  let (schema : tuples) = lines s
      tuples1 = filter ((> 0) . length) tuples
  in (parseSchema schema, map parseTuple tuples1)

parseTuples = map parseTuple . lines

