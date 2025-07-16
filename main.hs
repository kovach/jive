import Prelude hiding (Word)
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

-- previous term, new term -> prefix, new term
step :: Maybe Temp -> Temp -> M (Maybe Temp)

step Nothing t = pure $ Just t

step (Just (TempAtom p v 0)) t = tell [finish p v] >> step Nothing t

step (Just (TempVar v)) (TempAtom p vs a) =
  let vs' = v : vs in
  if a > 1 then pure $ Just (TempAtom p vs' (a-1))
           else tell [finish p vs'] >> pure Nothing

step (Just a@(TempAtom _ _ _)) v@(TempVar _) = step (Just v) a

step (Just (TempAtom p1 t1 a1)) (TempAtom p2 t2 a2) | a1 == 1 = do
  v <- fresh
  let t1' = v : t1
  let t2' = v : t2
  tell [finish p1 t1']
  pure $ Just $ TempAtom p2  t2' (a2-1)

step (Just x1@(TempAtom p1 t1 a1)) x2@(TempAtom p2 t2 a2) | a1 > 1 && a2 == 1 = step (Just x2) x1

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
run2 s = run1 s . map parse . words

parse :: String -> Word
parse s@(x : _) | isUpper x = WVar s
parse s@(x : _) = WPred s

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
eg5 = run2 sch "cat cat"

bad1 = run2 sch "cat on" -- bad
bad2 = run2 sch "on on" -- bad
