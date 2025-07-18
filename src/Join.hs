module Join where

import Prelude hiding ( lookup )
import qualified Prelude as P
import Types

import Data.Maybe
import Control.Monad ( foldM )

emptyBinding = Binding []
extend (Binding subst) v l = Binding ((v,l) : subst)
bindLookup k (Binding bs) = P.lookup k bs
toList (Binding bs) = bs

toPattern (Atom p ts) = Pattern p ts

type DB = [Tuple]

unifyPattern :: DB -> Pattern -> [Binding]
unifyPattern db (Pattern p ts) = mapMaybe ok db
  where
    n = length ts
    ok (Tuple p' vs) =
      if p == p' && length vs == n then foldM unify emptyBinding (zip ts vs) else Nothing
    unify subst (TermVar v, l) = Just (extend subst v l)
    unify subst (TermLit l', l) = if l' == l then Just subst else Nothing

bjoin :: Binding -> Binding -> Maybe Binding
bjoin b1 b2 = foldM step b1 $ toList b2
  where
    step subst (k,v) =
      case bindLookup k subst of
        Nothing -> pure $ extend subst k v
        Just v' -> if v == v' then pure subst else Nothing

bjoins :: [Binding] -> [Binding] -> [Binding]
bjoins as bs = flip concatMap as (\a -> mapMaybe (bjoin a) bs)

joins :: DB -> [Pattern] -> [Binding]
joins db ps = foldl bjoins [emptyBinding] (map (unifyPattern db) ps)
