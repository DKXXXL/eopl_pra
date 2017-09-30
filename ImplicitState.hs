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

data Exp =
  ConstExp Integer
  | ZeroTestExp Exp
  | CondExp Exp Exp Exp
  | DiffExp Exp Exp
  | VarExp Symbol
  | LetExp Symbol Exp Exp
  | ProcExp Symbol Exp
  | CallProcExp Exp Exp
  | SetExp Symbol Exp
  | Beginblock [Exp]


value_of :: Exp -> Environment -> Store -> (ExpVal, Store)
value_of (ConstExp i) _ st = (NumVal i, st)
value_of (ZeroTestExp e) env st =
  let ((NumVal i), st0) = value_of e env st
  in BoolVal (i == 0) env st0
 -- I don't want to do this any more.
