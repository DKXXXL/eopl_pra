#lang eopl

(define-datatype Environment environment?
  (empty)
  (extend-env
   (sym symbol?)
   (bind (lambda (x) #t))
   (env environment?)))

(define (apply-env sym env)
  (cases Environment env
    (empty () `UNBOUND_SYMBOL)
    (extend-env (var binding env_)
                (if (eq? var sym)
                    binding
                    (apply-env sym env_)))))

(define-datatype Proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?)))

(define proc_decl_env
  (lambda (proc)
    (cases Proc proc
      (procedure (var body env) env)
      (else `UNEXPECTED_ERROR))))

(define proc_body
  (lambda (proc)
    (cases Proc proc
      (procedure (var body env) body)
      (else `UNEXPECTED_ERROR))))

(define proc_para
  (lambda (proc)
    (cases Proc proc
      (procedure (var body env) var)
      (else `UNEXPECTED_ERROR))))

(define-datatype Expval expval?
  (num-val
   (n number?))
  (bool-val
   (b boolean?))
  (proc-val
   (p proc?)))


(define (expval->num expv)
  (cases Expval expv
    (num-val (n) n)
    (else `TYPE_INCONSISTENT)))

(define (expval->bool expv)
  (cases Expval expv
    (bool-val (n) n)
    (else `TYPE_INCONSISTENT)))

(define (expval->proc expv)
    (cases Expval expv
      (proc-val (n) n)
      (else `TYPE_INCONSISTENT)))



(define-datatype program program?
  (a-program
   (expl expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (zero?-exp
   (num expression?))
  (if-exp
   (judge expression?)
   (iftrue expression?)
   (iffalse expression?))
  (diff-exp
   (num1 expression?)
   (num2 expression?))
  (var-exp
   (varname symbol?))
  (let-exp
   (sym symbol?)
   (bind expression?)
   (expr expression?))
  (proc-exp
   (sym symbol?)
   (body expression?))
  (call-proc-exp
   (p expression?)
   (argu expression?)
  ))
; env :: [(symbol, expval)]
; value-of :: expression -> env -> expval
(define (value-of expres env)
  (cases expression expres
    (const-exp (n)
               (num-val n))
    (zero?-exp (exp)
               (let ([val0 (value-of exp env)])
                       (bool-val (eq? 0 (expval->num val0)))))
    (if-exp (judgement iftrue iffalse)
            (let ([judgement0 (value-of judgement env)])
              (if (expval->bool judgement0)
                  (value-of iftrue)
                  (value-of iffalse))))

    (diff-exp (a b)
              (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (num-val (- (expval->num num0) (expval->num num1)))))

    (var-exp (s)
             (env s))
    (let-exp (sym bind body)
             (let ([binding (value-of bind)])
               (value-of body (extend-env sym binding env))))
    (proc-exp (sym body)
              (proc-val (procedure sym body env)))

    (call-proc-exp (p argu)
                   (apply-procedure p argu env))
    
    (else `UNEXPECTED_PARAMETER)))
            

; part of value-of
; apply-procedure :: Exp -> Exp -> Expval
(define (apply-procedure proc_ argu_ current_env)
  (let ([proc (expval->proc (value-of proc_ current_env))]
        [argu (value-of argu_ current_env)])
    (value-of (proc_body proc) (extend-env (proc_para proc) argu (proc_decl_env proc)))))


