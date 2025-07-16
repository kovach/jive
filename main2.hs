import Prelude hiding (Word, lex)
import Control.Monad (foldM)
import Control.Monad.State
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Char
import Data.Maybe

type Var = String -- "upper"
type Pred = String -- "lower"
data Word = WVar Var | WPred Pred
type Arity = Int
data Term -- todo
  = TermVar Var
  deriving (Eq)
data Atom = Atom Pred [Term]
  deriving (Eq)
data Temp = TempVar Var | TempAtom Pred [Var] Arity
  deriving (Eq)

pwrap x = "(" <> x <> ")"
instance Show Term where
  show (TermVar v) = v
instance Show Atom where
  show (Atom p ts) = p <> " " <> unwords (map show ts)
instance Show Temp where
  show (TempVar v) = v
  show (TempAtom p vs a) = pwrap (p <> " " <> unwords (map show (reverse vs))) <> "/" <> show a

type Schema = Pred -> Arity

type M a = WriterT [Atom] (State Int) a

fresh :: M Var
fresh = do
  s <- get
  modify (+1)
  pure $ show s

arity (TempVar _) = 1
arity (TempAtom _ _ a) = a

finish :: Pred -> [Var] -> M ()
finish p vs = tell [Atom p (map TermVar $ reverse vs)]

type Stack a = [a]

-- Parse state. left side: progress, right side: input sentence.
type St = (Stack Temp, Stack Temp)

step :: St -> M St

-- If top of input is nullary, yield the finished atom
step (l, TempAtom p vs 0 : r) = finish p vs >> pure (l, r)

-- If top word of stack or input is var and other is atom,
--   bind the var and move to input position.
step (TempVar v : l,  TempAtom p vs a : r) =
  pure (l, TempAtom p (v : vs) (a - 1) : r)
step (TempAtom p vs a : l,  TempVar v : r) =
  pure (l, TempAtom p (v : vs) (a - 1) : r)

-- If one of p, q is unary, then
--   bind it to a fresh var, finish it, and bind the other to the same var
step (TempAtom p1 t1 1 : l,  TempAtom p2 t2 a2 : r) = do
  v <- fresh
  finish p1 (v : t1)
  pure (l, TempAtom p2 (v : t2) (a2 - 1) : r)
step (x1@(TempAtom _ _ a1) : l, x2@(TempAtom p2 t2 1) : r) = pure (x2 : l, x1 : r)

-- Otherwise, push the next word to the stack
step (l, w : r) = pure (w : l, r)

-- Done
step (s, []) = pure (s, [])

run1 :: Schema -> [Word] -> [Atom]
run1 s ws = snd . fst $ flip runState 0 $ runWriterT $ do
    out <- iter (wrap step) ([], map (load s) ws)
    let out' = last out
    case out' of
      ([], []) -> pure ()
      _ -> error $ "bad parse. temp term remaining:\n  " <> show out'
    pure ()

wrap :: (Eq a, Monad m) => (a -> m a) -> a -> m (Maybe a)
wrap f x = do
  x' <- f x
  pure $ if x == x' then Nothing else Just x'

iter :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iter f v = do
  v' <- f v
  case v' of
    Nothing -> pure [v]
    Just v' -> do
      r <- iter f v'
      pure (v : r)

load :: Schema -> Word -> Temp
load s (WVar v) = TempVar v
load s (WPred p) = TempAtom p [] (s p)

run2 :: Schema -> String -> [Atom]
run2 s = run1 s . map parse . lex

lex :: String -> [String]
lex = words . concatMap fix
  where
    fix '(' = " ( "
    fix ')' = " ) "
    fix c = [c]

parse :: String -> Word
parse s@(x : _) | isUpper x = WVar s
parse s@(x : _) = WPred s
parse [] = error "empty word"

sch :: Schema
sch = fromJust . flip lookup
  [ ("on", 2), ("with", 2)
  , ("sees", 3)
  , ("cat", 1), ("shelf", 1), ("telescope", 1)
  ]

test = run2 sch

eg1 = "cat on shelf"
eg1' = "on cat shelf"
eg2 = "cat on shelf shelf on cat"
eg3 = "cat sees cat with telescope" -- surprising?
eg5 = "cat cat cat cat"
eg6 = "cat X X on Y"
eg7 = "X cat on X Y"

bad1 = "cat on" -- bad, incomplete
bad2 = "on on" -- bad, incomplete

-- confusing examples
conf1 = "on on cat cat"  -- a cat is on something that is on a cat
conf1' = "on cat on cat" -- a cat is on something that is on a cat
conf2 = "X Y on" -- Y is on X

tests = [eg1, eg1', eg2, eg3, eg5, eg6, eg7, conf1, conf2]

ptest t =
  putStrLn $ show t
  <> ":\n"
  <> unlines (map show (test t))

chk = mapM_ ptest tests

{-

# Notes

# Todo
  - parens
  - articles

-}
