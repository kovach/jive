module Types where

import Data.List ( intercalate )

type Var = String
type Pred = String
type Sym = String
type Arity = Int
data Literal = LitInt Int | LitSym Sym
  deriving (Eq)
data Term -- todo
  = TermVar Var
  | TermLit Literal
  deriving (Eq)
data Atom = Atom Pred [Term]
  deriving (Eq)
data Word = WVar Var | WPred Pred | WPush | WPop
data Temp
  -- a variable
  = TempVar Var
  -- a partially applied atom with `Arity` missing arguments
  -- vars are in reverse order
  | TempAtom Pred [Var] Arity
  | Temps [Temp]
  | Push | Pop
  deriving (Eq)

instance Num Literal where
  fromInteger = LitInt . fromIntegral

pwrap x = "(" <> x <> ")"
instance Show Literal where
  show (LitInt i) = show i
  show (LitSym s) = show s
instance Show Term where
  show (TermVar v) = v
  show (TermLit l) = show l
instance Show Atom where
  show (Atom p ts) = p <> " " <> unwords (map show ts)
instance Show Temp where
  show (TempVar v) = v
  show (TempAtom p vs a) = pwrap (p <> " " <> unwords (map show (reverse vs))) <> "/" <> show a
  show (Temps ts) = "[" <> intercalate " | " (map show ts) <> "]"
  show Push = "push"
  show Pop = "pop"

data Pattern = Pattern Pred [Term]
  deriving Show
data Tuple = Tuple Pred [Literal]
instance Show Tuple where
  show (Tuple p ts) = p <> " " <> unwords (map show ts)
newtype Binding = Binding [(Var, Literal)]

instance Show Binding where
  show (Binding bs) = unlines $
    ["{"]
    <> map (\(k,v) -> "  " <> k <> ": " <> show v) bs
    <> ["}"]

type Schema = Pred -> [Arity]

