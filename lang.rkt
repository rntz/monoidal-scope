#lang racket

(require (prefix-in racket: racket))

(require "util.rkt")

(define (fix f) (lambda args (apply f f args)))

(define (literal? x)
  (or (string? x) (number? x)))

(define (eval exp [env base-env])
  (match exp
    [`(lambda ,params . ,body)
      (lambda args (eval-body body (env-bind env params args)))]
    ;; TODO: let
    [`(begin . ,body) (eval-body body env)]
    [`(import . ,_) (error "invalid use of import in expression")]
    [`(module . ,_) (error "invalid use of module in expression")]
    [`(define . ,_) (error "invalid use of define in expression")]
    [`(quote ,x) x]
    [`(,f . ,as)
      (apply (eval f env) (map (lambda (x) (eval x env)) as))]
    [(? symbol? x) (env-get env x)]
    [(? literal? x) x]))

;; Returns (values V E), where V is the value of the decl-or-expr, and E is the
;; extension the declaration makes to the current environment.
(define (eval-decl exp env)
  ;; TODO: module declarations
  ;; TODO: monoid declarations
  (match exp
    [`(import ,name)
      (let ((mod (env-get env name)))
        (values mod (nodule-env mod)))]
    [`(module ,name . ,body)
      (let-values ([(val mod-env) (eval-module-body body env)])
        (let ([mod (make-nodule name mod-env)])
          (values mod (env-single name mod))))]
    [`(define ,(? symbol? name) . ,body)
      (let ((val (eval-body body env)))
        (values val (env-single name val)))]
    [`(define (,name . ,params) . ,body)
      (define (self . args)
        (eval-body body (env-bind (env-put env name self) params args)))
      (values self (env-single name self))]
    [exp (values (eval exp env) env-empty)]))

;; Returns (values V E), where V is the value of the last decl-or-expr in the
;; module, and E is the environment the module defines.
(define (eval-module-body body env)
  (let loop ([val (void)]
             [body body]
             ;; Need to separate module env from inherited env so that we don't
             ;; re-export things not defined in the module.
             [env env]
             [mod-env env-empty])
    (match body
      ['() (values val mod-env)]
      [(cons exp rest)
        (let-values ([(val decl-env) (eval-decl exp env)])
          (loop val rest
            (env-join env decl-env)
            (env-join mod-env decl-env)))])))

(define (eval-body body [env base-env])
  (let-values ([(val mod-env) (eval-module-body body env)])
    val))


;; Modules, called nodule to avoid conflicting with racket module.
(struct nodule (name env) #:prefab #:constructor-name make-nodule)


;; An environment maps identifiers to values. It separately maps identifiers to
;; monoids. If an ident is mapped to a monoid, that monoid is used to combine
;; values bound to that identifier.

;; TODO: monoid stuff
(struct env (map) #:prefab #:constructor-name make-env)

(define env-empty (make-env (hash)))
(define (env-join a b)
  ;; TODO: monoid-based merging
  (make-env (dict-union (env-map a) (env-map b))))

(define (env-single name val) (make-env (hash name val)))
(define (env-from-list bindings)
  (reduce (map (lambda (x) (apply env-single x)) bindings)
    env-empty env-join))

(define (env-get env name) (dict-ref (env-map env) name))
(define (env-put env name val) (env-join env (env-single name val)))

(define (env-bind env names vals)
  (when (not (= (length names) (length vals)))
    (error "wrong number of parameters to bind"))
  (for/fold ([env env]) ([name names] [val vals])
    (env-put env name val)))


;; The base environment to inherit from Racket
(define prelude
  '(+ - * /
     equal?
     apply
     list cons car cdr null?
     print display displayln
     map foldl foldr
     string-append
     ))

(define base-env
  (env-from-list (map (lambda (name) `(,name ,(racket:eval name))) prelude)))
