
type Symbol = Integer

data Dictionary key value = 
    EmptyDict 
    | ExtendDict key value (Dictionary key value)
    deriving Eq

f :: Eq a => a -> a -> Bool 
f a b = a == b

data ExpValue = NumVal Integer | BoolVal Bool | ProcVal Symbol Exp Environment

type Environment = Dictionary Symbol ExpValue

data Exp =
    ConstExp Integer 
    | ZeroTestExp Exp
    | CondExp Exp Exp Exp 
    | DiffExp Exp Exp 
    | VarExp Symbol 
    | LetExp Symbol Exp Exp 
    | ProcExp Symbol Exp 
    | CallProcExp Exp Exp

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

value_of :: Exp -> Environment -> ExpValue
value_of (ConstExp i) env = NumVal i
value_of (ZeroTestExp e0) env =
    let (NumVal integerValue) = value_of e0 env 
    in  BoolVal (integerValue == 0)
value_of (CondExp judgement iftrue ifelse) env =
    let (BoolVal judge) = value_of judgement env 
    in if judge 
       then (value_of iftrue env)
       else (value_of ifelse env)
value_of (DiffExp a b) env =
    let (NumVal n0) = value_of a env
        (NumVal n1) = value_of b env 
    in NumVal (n0 - n1)
value_of (VarExp s) env =
    let (Just v) = (applyEnv env s)
    in v
value_of (LetExp s v body) env =
    let bind = value_of v env
    in value_of body (ExtendDict s bind env)
value_of (ProcExp s body) env =
    ProcVal s body env
value_of (CallProcExp proc argu) env =
    let (ProcVal s body bind_env) = value_of proc env
        argument = value_of argu env 
    in (value_of body (ExtendDict s argument bind_env))