type Symbol = Integer

data Dictionary key value = 
    EmptyDict 
    | ExtendDict key value (Dictionary key value)
    deriving (Show, Eq)

data ExpVal = 
    NumVal Integer 
    | BoolVal Bool 
    | ProcVal Symbol Exp Environment 
    | RecFVal Symbol Exp Environment
    | TERMINATION String
    deriving Show

type Environment = Dictionary Symbol ExpVal

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


data Cont x y =
  EndCont
  | Cont (x -> y) (Cont x y)

applyCont :: Cont ExpVal ExpVal -> ExpVal -> ExpVal
applyCont EndCont x = x
applyCont (Cont f c) x = apply c (f x)

value_of :: Exp -> Environment -> Cont ExpVal ExpVal -> ExpVal
value_of :: Exp -> Environment -> Cont ExpVal ExpVal -> ExpVal
value_of (ConstExp i) env cont =
  applyCont cont (NumVal i)
value_of (ZeroTestExp e) env cont =
  value_of e env
  (Cont (\(NumVal i) -> BoolVal (i == 0)) cont)
  -- (Cont (\(NumVal i) -> applyCont cont (BoolVal (i == 0))))

value_of (CondExp judge e0 e1) env cont =
  value_of judge env
  (Cont (\(BoolVal b) -> if b
                         then value_of e0 env cont
                         else value_of e1 env cont))

value_of (DiffExp e0 e1) env cont =
  value_of e0 env
  (Cont (\(NumVal n0) ->
          value_of e1 env
          (Cont (\(NumVal n1) ->
                  applyCont cont (NumVal (n0 - n1))))))
value_of (VarExp s) env cont =
  let (Just v) = applyEnv env s
  in case v of (RecFVal x body env') -> applyCont cont (ProcVal x body (ExtendDict s v env'))
               _ -> applyCont cont v
               
value_of (LetExp s bind body) env cont =
  value_of bind env
  (Cont (\bind_v ->
          value_of body (ExtendDict s bind_v env) cont))

value_of (ProcExp s body) env cont =
  applyCont cont (ProcVal s body env)

value_of (CallProcExp e1 e2) env cont =
  value_of e1 env
  (Cont (\ (ProcVal para body env') ->
          value_of e2 env
          (Cont (\ x ->
            value_of body (ExtendDict para x env') cont))))

value_of (LetRecExp funName s body after) env cont =
  value_of after (ExtendDict funName (RecFVal s body env) env) cont
  

