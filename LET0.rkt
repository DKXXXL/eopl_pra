#lang eopl


(define (extend-env sym bind env)
  (lambda (lookfor)
    (if (eq? lookfor sym)
        bind
        (env lookfor))))

(define empty-env (lambda (x) `UNBOUND_VARIABLE))

(define-datatype Expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(define (expval->num expv)
  (cases Expval expv
    (num-val (n) n)
    (else `TYPE_INCONSISTENT)))

(define (expval->bool expv)
  (cases Expval expv
    (bool-val (n) n)
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
   (expr expression?)))
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

    (else `UNEXPECTED_PARAMETER)))
            