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
data Term = TermVar Var
  deriving (Show, Eq)
data Atom = Atom Pred [Term]
  deriving (Show, Eq)
data Temp = TempVar Var | TempAtom Pred [Var] Arity
  deriving (Show, Eq)

type Schema = Pred -> Arity

type M a = WriterT [Atom] (State Int) a

fresh :: M Var
fresh = do
  s <- get
  modify (+1)
  pure $ show s

arity (TempVar _) = 1
arity (TempAtom _ _ a) = a

finish :: Pred -> [Var] -> Atom
finish p vs = Atom p (map TermVar $ reverse vs)

-- previous partial atom, new word -> new partial atom (and `tell` any completed atoms)
step :: Maybe Temp -> Temp -> M (Maybe Temp)

-- begin a new atom (t)
step Nothing t =
  pure $ Just t

-- handle an atom that was completed at the previous step
step (Just (TempAtom p v 0)) t =
  tell [finish p v] >> step Nothing t

-- if current temp is Var, bind it to incoming atom
step (Just (TempVar v)) (TempAtom p vs a) =
  pure $ Just (TempAtom p (v : vs) (a - 1))
step (Just a@(TempAtom _ _ _)) v@(TempVar _) = step (Just v) a

-- if one of p, q is unary, then
--   p q -> p V q V
step (Just (TempAtom p1 t1 1)) (TempAtom p2 t2 a2) = do
  v <- fresh
  tell [finish p1 (v : t1)]
  pure $ Just $ TempAtom p2 (v : t2) (a2-1)
step (Just x1@(TempAtom p1 t1 a1)) x2@(TempAtom p2 t2 1) | a1 > 1 = step (Just x2) x1

-- not allowed: (var, var) or (x, y) with arity x < 1 or arity y < 1
step a b  = error $ "bad sequence: " <> show a <> " / " <> show b -- incorrect

run1 :: Schema -> [Word] -> [Atom]
run1 s ws = snd . fst $ flip runState 0 $ runWriterT $ do
    x <- foldM step Nothing (map (load s) ws)
    case x of
      Nothing -> pure ()
      Just (TempAtom p t 0) -> tell [finish p t]
      _ -> error $ "bad parse. temp term remaining: " <> show x
    pure ()

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

eg1 = run2 sch "cat on shelf"
eg1' = run2 sch "on cat shelf"
eg2 = run2 sch "cat on shelf shelf on cat"
eg3 = run2 sch "cat sees cat with telescope" -- surprising?
eg5 = run2 sch "cat cat cat cat"
eg6 = run2 sch "cat X X on Y"
eg7 = run2 sch "X cat on X Y"

bad1 = run2 sch "cat on" -- bad, incomplete
bad2 = run2 sch "on on" -- bad, cannot combine two arity-2 words

{-
# Notes

# Todo
  - parens
  - articles
-}
