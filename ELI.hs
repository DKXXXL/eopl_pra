-- Abstract Interpretation

module ELI where

import LETREC0
import Prelude hiding (take)


type Address = Integer

data NLExp = 
    NLConstExp Integer
    | NLZeroTestExp NLExp
    | NLCondExp NLExp NLExp NLExp
    | NLDiffExp NLExp NLExp
    | NLVarExp Address
    | NLLetExp NLExp NLExp
    | NLProcExp NLExp
    | NLCallProcExp NLExp NLExp
    | NLLetRecExp NLExp NLExp
      deriving Show

type NameEnv = [Symbol]

lookfor :: Eq a => a -> [a] -> Integer
lookfor x [] = error "Unfound Value"
lookfor x (y : b) = if (x == y) then 0 else (1 + lookfor x b)

take :: Integer -> [a] -> a
take 0 (x:_) = x
take i (_:t) = take (i - 1) t

translate :: NameEnv -> Exp -> NLExp
translate env (ConstExp i) = NLConstExp i
translate env (ZeroTestExp e) = NLZeroTestExp (translate env e)
translate env (CondExp judge e1 e2) = NLCondExp (translate env judge) (translate env e1) (translate env e2)
translate env (DiffExp e1 e2) = NLDiffExp (translate env e1) (translate env e2)
translate env (VarExp s) = NLVarExp (lookfor s env)
translate env (LetExp s bind body) = NLLetExp (translate env bind) (translate (s: env) body)
translate env (ProcExp x body) = NLProcExp (translate (x : env) body)
translate env (CallProcExp e1 e2) = NLCallProcExp (translate env e1) (translate env e2)
translate env (LetRecExp funname x fun after) = NLLetRecExp (translate (x:funname : env) fun) (translate (funname : env) after)
translate _ _ = error "Unexpected Error : translate"

type ExpStack = [NLExpValue]

data NLExpValue = 
    NLNumVal Integer 
    | NLBoolVal Bool 
    | NLProcVal NLExp ExpStack 
    | NLRecFVal NLExp ExpStack
    | TERMINATION String
    deriving Show


value_of :: NLExp -> ExpStack -> NLExpValue
value_of (NLConstExp i) env = NLNumVal i
value_of (NLZeroTestExp e0) env =
  let (NLNumVal i) = value_of e0 env
  in (NLBoolVal (i == 0))
value_of (NLCondExp judge e1 e2) env =
  let (NLBoolVal b) = value_of judge env
  in if b then (value_of e1 env) else (value_of e2 env)
value_of (NLDiffExp e1 e2) env =
  let (NLNumVal n1) = value_of e1 env
      (NLNumVal n2) = value_of e2 env
  in NLNumVal (n1 - n2)
value_of (NLVarExp i) env =
  let nlval = take i env
  in case nlval of (NLRecFVal func env') -> (NLProcVal func (nlval : env'))
                   _ -> nlval
value_of (NLLetExp bind body) env =
  let bind_val = value_of bind env
  in value_of body (bind_val : env)
value_of (NLProcExp body) env =
  NLProcVal body env
value_of (NLCallProcExp fun arg) env =
  let argument = value_of arg env
      (NLProcVal funbody env') = value_of fun env
  in value_of funbody (argument : env')
value_of (NLLetRecExp bindrec after) env =
  let bindrecf = NLRecFVal bindrec env
  in value_of after (bindrecf : env)
value_of _ _ = error "Unexpected Input."
