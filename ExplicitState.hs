module ExplicState where

type Symbol = String

data Dictionary key value = 
    EmptyDict 
    | ExtendDict key value (Dictionary key value)
    deriving (Show, Eq)

type Environment = Dictionary Symbol ExpVal

applyDict :: Eq k => Dictionary k v -> k -> Maybe v
applyDict EmptyDict _ = Nothing
applyDict (ExtendDict key value tail) target = 
        (if (key == target) 
         then   (Just value) 
         else   ((applyEnv tail target)))
{- I had to let applyEnv deduce type it self -}
{- Weird -}
{- applyEnv :: Environment -> Symbol -> Maybe ExpValue -}
applyEnv x y = applyDict x y

type Location = Integer
type Store = (Location, [ExpVal])

emptyStore = (-1, [])

setStore :: Store -> Location -> ExpVal -> Store
setStore (currentLoc, storage) i newvalue =
  if (currentLoc < i)
  then (error "Unexpected behaviour")
  else (currentLoc, setList (currentLoc - i) storage newvalue)
  where setList :: Integer -> [a] -> a -> [a]
        setList 0 (x:t) r = r:t
        setList i (x:t) r = x : (setList (i - 1) t r)

getStore :: Store -> Location -> ExpVal
getStore (currentLoc, storage) i =
  if (currentLoc < i)
  then (error "Storage Unfound.")
  else getList (currentLoc - i) storage
  where getList :: Integer -> [a] -> a
        getList 0 (x : _) = x
        getList i (_ : t) = getList (i - 1) t

data ExpVal =
  NumVal Integer
  | BoolVal Bool
  | ProcVal Symbol Exp Environment
  | RefVal Integer 

data Exp =
  ConstExp Integer
  | ZeroTestExp Exp
  | CondExp Exp Exp Exp
  | DiffExp Exp Exp
  | VarExp Symbol
  | LetExp Symbol Exp Exp
  | ProcExp Symbol Exp
  | CallProcExp Exp Exp
  | NewrefExp Exp
  | DerefExp Exp
  | SetrefExp Exp Exp
  | Beginblock [Exp]


value_of :: Exp -> Environment -> Store -> (ExpVal, Store)
value_of (ConstExp i) env st = (NumVal i, st)
value_of (ZeroTestExp e) env st =
  let ((NumVal i), st0) = value_of e env st
  in (BoolVal (i == 0), st0)
value_of (CondExp judge e0 e1) env st =
  let ((BoolVal b), st0) = value_of judge env st
  in if b
     then (value_of e0 env st0)
     else (value_of e1 env st0)
value_of (DiffExp e0 e1) env st =
  let ((NumVal n0), st0) = value_of e0 env st
  in let ((NumVal n1), st1) = value_of e1 env st0
     in (NumVal (n0 - n1), st1)
value_of (VarExp s) env st =
  let (Just x) = applyEnv env s
  in (x, st)
value_of (LetExp s bind body) env st =
  let (bindval, st0) = value_of bind env st
  in value_of body (ExtendDict s bindval env) st0
value_of (ProcExp s body) env st =
  (ProcVal s body env, st)
value_of (CallProcExp f arg) env st =
  let ((ProcVal para body env'), st0) = value_of f env st
  in let ( argument, st1) = value_of arg env st0
     in value_of body (ExtendDict para argument env') st1
value_of (NewrefExp e) env st =
  let (initialvalue, st0) = value_of e env st
  in let (loc, storage) = st0
     in (RefVal (loc + 1), (loc + 1, initialvalue : storage))
value_of (DerefExp e) env st =
  let ((RefVal i), st0) = value_of e env st
  in (getStore st0 i, st0)
value_of (SetrefExp ref v) env st =
  let ((RefVal i), st0) = value_of ref env st
  in let ( newval , st1) = value_of v env st
     in (newval, setStore st1 i newval)
value_of (Beginblock (x:[])) env st =
  value_of x env st
value_of (Beginblock (x:y)) env st =
  let (_ , st0) = value_of x env st
  in value_of (Beginblock y) env st0
     

    
