
type Symbol = Integer

data Dictionary key value = 
    EmptyDict 
    | ExtendDict key value (Dictionary key value)
    deriving (Show, Eq)

f :: Eq a => a -> a -> Bool 
f a b = a == b

data ExpValue = 
    NumVal Integer 
    | BoolVal Bool 
    | ProcVal Symbol Exp Environment 
    | RecFVal Symbol Exp Environment
    | RecFsVal [(Symbol, Symbol, Exp)] Environment
    | TERMINATION String
    deriving Show

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
    | LetRecExp Symbol Symbol Exp Exp 
    | LetRecExps [(Symbol, Symbol, Exp)] Exp
    deriving Show

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

extendAll :: [(Symbol, ExpValue)] -> Environment -> Environment
extendAll ((s, e) : l) env = extendAll l (ExtendDict s e env)
extendAll [] env = env 

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
value_of (LetExp s v body) env =
    let bind = value_of v env
    in value_of body (ExtendDict s bind env)

value_of (ProcExp s body) env =
    ProcVal s body env
value_of (CallProcExp proc argu) env =
    let (ProcVal s body bind_env) = value_of proc env
        argument = value_of argu env 
    in (value_of body (ExtendDict s argument bind_env))
value_of (LetRecExp funName param body after) env =
    value_of after
        (ExtendDict funName (RecFVal param body env) env)
value_of (LetRecExps recurList after) env =
    let fun_names = map (\ (a, _, _) -> a) recurList
        recurs_val = RecFsVal recurList env
    in value_of after (extendAll (zip fun_names (repeat recurs_val)) env)

value_of (VarExp s) env =
    let (Just v) = (applyEnv env s)
    in let getV :: Symbol -> [(Symbol, Symbol, Exp)] -> (Symbol, Exp)
           getV t = (\(name ,para ,body) -> (para, body)) 
                    . head . filter ((== t) . (\ (a, _, _) -> a))
    in case v of (RecFVal param body env') -> (ProcVal param body (ExtendDict s (RecFVal param body env') env'))
                 (RecFsVal list_dep env') -> 
                    let (para, body) = getV s list_dep
                        fun_names = map (\ (a, _, _) -> a) list_dep
                    in (ProcVal para body (extendAll (zip fun_names (repeat v)) env'))
                 (_) -> v
value_of u _ = 
    error ("Unexpected Expression:" `mappend` (show u))