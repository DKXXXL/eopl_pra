#lang eopl

(define (extend-env sym bind env)
  (lambda (lookfor)
    (if (eq? lookfor sym)
        bind
        (env lookfor))))

(define empty-env (lambda (x) `UNBOUND_VARIABLE))


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