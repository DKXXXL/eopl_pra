
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
   (bool boolean?))
  (pair-val
   (fst expval?)
   (tail expval?))
  (emptylist-val)
  )

(define (expval->num expv)
  (cases Expval expv
    (num-val (n) n)
    (else `TYPE_INCONSISTENT)))

(define (expval->bool expv)
  (cases Expval expv
    (bool-val (n) n)
    (else `TYPE_INCONSISTENT)))

(define (expval->pair expv)
  (cases Expval expv
    (pair-val (a b) (cons a b))
    (else `TYPE_INCONSISTENT)))


(define-datatype program program?
  (a-program
   (expl expression?)))

(define (list-of-sth sth?)
  (define (LP? l)
    (or (eq? l '())
    (and (pair? l)
         (and (sth? (car l))
              (LP? (cdr l))))))
  LP?)

(define (pair-of-a-b a? b?)
  (define (predic p)
    (and (pair? p)
         (a? (car p))
         (b? (cdr p))))
  predic
  )

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
  (neg-exp
   (num expression?))
  (add-exp
   (num0 expression?)
   (num1 expression?))
  (mult-exp
   (num0 expression?)
   (num1 expression?))
  (divide-exp
   (num0 expression?)
   (num1 expression?))
  (equal-exp
   (num0 expression?)
   (num1 expression?))
  (greater-exp
   (num0 expression?)
   (num1 expression?))
  (less-exp
   (num0 expression?)
   (num1 expression?))
  (cons-exp
   (e1 expression?)
   (e2 expression?))
  (car-exp
   (e expression?))
  (cdr-exp
   (e expression?))
  (null?-exp
   (e expression?))
  (emptylist-exp)
  (list-exp
   (le (list-of-sth expression?)))
  (cond-exp
   (lpe (list-of-sth (pair-of-a-b expression? expression?))))
  
  )
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

    (neg-exp (n)
             (num-val (- (expval->num (value-of n env)))))

    (add-exp (a b)
              (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (num-val (+ (expval->num num0) (expval->num num1)))))
    (mult-exp (a b)
              (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (num-val (* (expval->num num0) (expval->num num1)))))

    (divide-exp (a b)
              (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (num-val (quotient (expval->num num0) (expval->num num1)))))

    (equal-exp (a b)
               (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (bool-val (equal? (expval->num num0) (expval->num num1)))))
    (greater-exp (a b)
               (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (bool-val (> (expval->num num0) (expval->num num1)))))
               
    (less-exp (a b)
               (let ([num0 (value-of a env)]
                    [num1 (value-of b env)])
                (bool-val (< (expval->num num0) (expval->num num1)))))

    (cons-exp (a b)
              (let ([fst (value-of a env)]
                    [snd (value-of b env)])
                (pair-val fst snd)))

    (car-exp (a)
             (car (expval->pair (value-of a env))))
    (cdr-exp (a)
             (cdr (expval->pair (value-of a env))))
    (null?-exp (a)
               (cases Expval (value-of a env)
                 (emptylist-val () (bool-val #t))
                 (else (bool-val #f))))
    (emptylist-exp ()
                   emptylist-val)
    (list-exp (l)
              (listexp->listexpval l))
    (cond-exp (l)
              (eval-condexp l env))
              
    
    (else `UNEXPECTED_PARAMETER)))

(define (listexp->listexpval le env)
  (cond
    [(pair? le) (pair-val (value-of (car le env))
                         (listexp->listexpval (cdr le) env))]
    [else (value-of le env)]))

(define (eval-condexp les env)
  (cond
    [(pair? les) (let ([cond-result (car les)]
                       [else-route (cdr les)])
                   (let ([condition (car cond-result)]
                         [result (cdr cond-result)])
                     (if (expval->bool (value-of condition env))
                         (value-of result env)
                         (eval-condexp else-route env))))]
    [else `ERROR-NO_RETURN_VAL]))
            
                                       
            