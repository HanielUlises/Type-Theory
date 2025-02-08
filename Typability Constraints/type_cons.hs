{-# LANGUAGE TupleSections #-}

-- Types
data Ty
  = TVar Int
  | TArr Ty Ty
  deriving (Show, Eq)

-- Generate a fresh type variable
fresh :: IO (IO Ty)
fresh = do
  n <- newIORef (-1)
  return $ do
    modifyIORef n (+1)
    TVar <$> readIORef n

-- Terms
data Term
  = Var String
  | Abs String Term
  | App Term Term
  deriving (Show)

-- Type constraints
type TEq = [(Ty, Ty)]

-- Type and equations
infer :: [(String, Ty)] -> Term -> IO (Ty, TEq)
infer env (Var x) = return (lookup x env `maybe` error "Unbound variable" , [])
infer env (Abs x t) = do
  f <- fresh
  ax <- f
  (at, et) <- infer ((x, ax) : env) t
  return (TArr ax at, et)
infer env (App t u) = do
  (at, et) <- infer env t
  (au, eu) <- infer env u
  f <- fresh
  ax <- f
  return (ax, (at, TArr au ax) : et ++ eu)
