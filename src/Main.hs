module Main where

import Prelude hiding (Word, lex)
import Control.Monad.State
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Char
import Data.Maybe

import Types
import Join

type Schema = Pred -> Arity

type M a = WriterT [Atom] (State Int) a

fresh' :: M Var
fresh' = do
  s <- get
  modify (+1)
  pure $ show s

fresh :: String -> M Var
fresh str = do
  s <- get
  modify (+1)
  pure $ str <> show s

finish :: Pred -> [Var] -> M ()
finish p vs = tell [Atom p (map TermVar $ reverse vs)]

type Stack a = [a]

-- Parse state. left side: progress, right side: input sentence.
type St = (Stack Temp, Stack Temp)

step :: St -> M St

-- [paren handling]
--   these steps are incidental to the algorithm
step (l, Push : r) = pure (Push : l, r)
step (x : Push : l, Pop : r) = pure (l, x : r)

-- [reduction step]
--
-- If top of input is nullary, yield the finished atom
step (l, TempAtom p vs 0 : r) = finish p vs >> pure (l, r)

-- [join step]
--
-- NB regarding next four cases:
-- when we apply a TempAtom `t` to something, its arity decreases.
--   If it becomes zero, we need to finish it.
--   If it becomes one, we might need to join it with the next lower item on the stack.
-- Either case is handled (at the next call to step) by putting `t` at the top of the right side.

-- If both tops are partial atoms and at least one is unary,
--   bind the unary one to a fresh var `v` and finish it; also bind the other to `v`.
step (TempAtom p ps 1 : l,  TempAtom q qs arity : r) = do
  v <- fresh'
  finish p (v : ps)
  pure (l, TempAtom q (v : qs) (arity - 1) : r)
-- swap and apply the first case
step (x1@(TempAtom _ _ _) : l, x2@(TempAtom _ _ 1) : r) = pure (x2 : l, x1 : r)

-- If one top is var and other is atom, bind the var.
-- Vars behave like unary predicates.
step (TempVar v : l,  TempAtom p vs a : r) =
  pure (l, TempAtom p (v : vs) (a - 1) : r)
step (TempAtom p vs a : l,  TempVar v : r) =
  pure (l, TempAtom p (v : vs) (a - 1) : r)

-- [push step]
--
-- Otherwise, push the next word to the stack
step (l, w : r) = pure (w : l, r)

-- [done]
step (s, []) = pure (s, [])

run1 :: Schema -> [Word] -> [Atom]
run1 s ws = snd . fst $ flip runState 0 $ runWriterT $ do
    out <- iter (wrap step) ([], map (load s) ws)
    let out' = last out
    case out' of
      ([], []) -> pure ()
      _ -> error $ "bad parse. temp term remaining:\n  " <> show out'
    pure ()

load :: Schema -> Word -> Temp
load _ (WVar v) = TempVar v
load _ WPush = Push
load _ WPop = Pop
load s (WPred p) = TempAtom p [] (s p)

wrap :: (Eq a, Monad m) => (a -> m a) -> a -> m (Maybe a)
wrap f x = do
  x' <- f x
  pure $ if x == x' then Nothing else Just x'

iter :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iter f v = do
  v' <- f v
  case v' of
    Nothing -> pure [v]
    Just v'' -> do
      r <- iter f v''
      pure (v : r)

run2 :: Schema -> String -> [Atom]
run2 s = run1 s . map tokenize . lex

lex :: String -> [String]
lex = words . concatMap fix
  where
    -- todo
    fix '(' = " ( "
    fix ')' = " ) "
    fix c = [c]

tokenize :: String -> Word
tokenize "(" = WPush
tokenize ")" = WPop
tokenize s@(x : _) | isUpper x = WVar s
tokenize s@('?' : _) = WVar s
tokenize s@(_ : _) = WPred s
tokenize [] = error "empty word"

sch1 :: Schema
sch1 = fromJust . flip lookup
  [ ("on", 2), ("with", 2)
  , ("sees", 3)
  , ("cat", 1), ("shelf", 1), ("telescope", 1)
  ]

test = run2 sch1

eg1 = "cat on shelf"
eg1' = "on cat shelf"
eg2 = "cat on shelf shelf on cat"
eg3 = "cat sees cat with telescope" -- surprising?
eg5 = "cat cat cat cat"
eg6 = "cat X X on Y"
eg6' = "cat ?x ?x on Y"
eg7 = "X cat on X Y"

bad1 = "cat on" -- bad, incomplete
bad2 = "on on" -- bad, incomplete

-- confusing examples
conf1 = "on on cat cat"  -- a cat is on something that is on a cat
conf1' = "on cat on cat" -- a cat is on something that is on a cat
conf2 = "X Y on" -- Y is on X

tests = [eg1, eg1', eg2, eg3, eg5, eg6, eg6', eg7, conf1, conf1', conf2]

ptest t = do
  let q = test t
  putStrLn $ show t
    <> ":\n"
    <> unlines (map show q)

chk1 = mapM_ ptest tests

toSchema :: [(Pred, Arity)] -> Schema
toSchema m  k =
  case flip lookup m k of
    Just v -> v
    Nothing -> error $ "undefined predicate: " <> k

tst q db = (q', joins q'' db' [[]])
  where
    q' = run2 (toSchema arities) q
    q'' = map toPattern $ q'
    (arities, db') = parseDb db

chk q = do
  db <- readFile "db"
  let (q', result) = tst q db
  print q'
  mapM_ print result

main = do
  putStrLn "example parses:"
  chk1
  putStrLn "example queries:"
  chk "cat saw X with Z"
  chk "(friend X) on Y"

{-

# Notes

# Todo
  - articles
-}

