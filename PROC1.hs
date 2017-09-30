
type Symbol = [Char]

data Dictionary key value = 
    EmptyDict 
    | ExtendDict key value (Dictionary key value)
    deriving Eq

applyDict :: Eq key => Dictionary key value -> key -> Maybe value
applyDict EmptyDict  _ = None
applyDict (ExtendDict k v tail) target = 
    if (k == target) 
        (Some v) 
        (Some (applyEnv tail target))

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

applyEnv :: Environment -> Symbol -> ExpValue
applyEnv = applyDict

value_of :: Exp -> Environment -> ExpValue
value_of (ConstExp i) env = NumVal i
value_of (ZeroTestExp e0) env =
    let (NumVal integerValue) = value_of e0 env 
    in  BoolVal (integerValue == 0)
value_of (CondExp judgement iftrue ifelse) env =
    let (BoolVal judge) = value_of judgement env 
    in if judge 
        (value_of iftrue env)
        (value_of iffalse env)
value_of (DiffExp a b) env =
    let (NumVal n0) = value_of a env
        (NumVal n1) = value_of b env 
    in NumVal (n0 - n1)
value_of (VarExp s) env =
    (applyEnv env s)
value_of (LetExp s v body) env =
    let bind = value_of v env
    in value_of body (ExtendDict s bind env)
value_of (ProcExp s body) env =
    ProcVal s body env
value_of (CallProcExp proc argu) env =
    let (ProcVal s body bind_env) = value_of proc env
        argument = value_of argu env 
    in (value_of body (ExtendDict s argument bind_env))