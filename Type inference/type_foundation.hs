type Var = String

-- Types
data Ty
  = TVar String
  | Arr Ty Ty
  deriving (Eq, Show)

-- Terms
data Term
  = Var Var
  | App Term Term
  | Abs Var Ty Term
  deriving (Eq, Show)

data TypeError = TypeError deriving (Show)

type Env = [(Var, Ty)]

-- Type inference
infer :: Env -> Term -> Either TypeError Ty
infer env (Var x) =
  case lookup x env of
    Just ty -> Right ty
    Nothing -> Left TypeError
infer env (Abs x a t) =
  case infer ((x, a) : env) t of
    Right ty -> Right (Arr a ty)
    Left err -> Left err
infer env (App t u) =
  case infer env t of
    Right (Arr a b) -> check env u a >> Right b
    _ -> Left TypeError

-- Type checking
check :: Env -> Term -> Ty -> Either TypeError ()
check env t a =
  case infer env t of
    Right ty -> if ty == a then Right () else Left TypeError
    Left err -> Left err

-- Typability
typable :: Env -> Term -> Bool
typable env t = case infer env t of
  Right _ -> True
  Left _  -> False
