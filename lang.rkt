#lang racket

(require (prefix-in racket: racket))
(require racket/set)

(require "util.rkt")

(define (literal? x)
  (or (string? x) (number? x)))

(define (eval exp [env base-env])
  (match exp
    [`(import . ,_) (error "invalid use of 'import in expression")]
    [`(module . ,_) (error "invalid use of 'module in expression")]
    [`(monoidify . ,_) (error "invalid use of 'monoidify in expression")]
    [`(use . ,_) (error "invalid use of 'use in expression")]
    [`(define . ,_) (error "invalid use of 'define in expression")]
    [`(quote ,x) x]
    [`(lambda ,params . ,body)
      (lambda args (eval-body body (env-bind env params args)))]
    [`(let ,bindings . ,body)
      (eval-body body
        (env-bind env
          (map car bindings)
          (map (lambda (x) (eval (cadr x) env)) bindings)))]
    [`(begin . ,body) (eval-body body env)]
    [`(,f . ,as)
      (apply (eval f env) (map (lambda (x) (eval x env)) as))]
    [(? symbol? x) (env-get env x)]
    [(? literal? x) x]))

;; Returns (values V E), where V is the value of the decl-or-expr, and E is the
;; extension the declaration makes to the current environment.
(define (eval-decl exp env)
  (match exp
    [`(import ,name)
      (let ((mod (env-get env name)))
        (values (void) (nodule-env mod)))]
    [`(module ,name . ,body)
      (let-values ([(val mod-env) (eval-module-body body env)])
        (let ([mod (make-nodule name mod-env)])
          (values (void) (env-single name mod))))]
    [`(monoidify ,name ,join-exp ,empty-exp)
      (let* ([join  (eval join-exp env)]
             [empty (eval empty-exp env)]
             [mon   (make-monoid name join empty)])
        (values (void) (env-join
                         (env-single-monoid name mon)
                         (env-single name empty))))]
    [`(use ,expr)
      (eval-module-body (read-file (eval expr env)) env)]
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
(struct nodule (name env) #:transparent #:constructor-name make-nodule
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "<module ~a defines ~a>"
       (nodule-name self)
       (sort (hash-keys (env-vals (nodule-env self))) symbol<?)))])

;; Monoids, with uids attached so we can identify when two monoids are the same.
(struct monoid (name uid join empty) #:prefab
  #:constructor-name make-monoid-internal)

(define (make-monoid name join empty)
  (make-monoid-internal name (gensym name) join empty))

(define (monoid=? a b) (eq? (monoid-uid a) (monoid-uid b)))


;; An environment maps identifiers to values. It separately maps identifiers to
;; monoids. If an ident is mapped to a monoid, that monoid is used to combine
;; values bound to that identifier.
(struct env (vals monoids) #:prefab #:constructor-name make-env)

(define env-empty (make-env (hash) (hash)))
(define (env-join a b)
  (define (monoid-merge name a b)
    (if (monoid=? a b) a
      (error "oops! I don't know how to merge monoids, sorry :(")))
  (let* ([monoids (hash-union (env-monoids a) (env-monoids b)
                    monoid-merge)]
          ;; TODO: check whether any monoids in b have mapped values in a. if
          ;; so, error out: adding a monoid to an identifier post-hoc is not
          ;; supported.
         [vals (hash-union (env-vals a) (env-vals b)
                 (lambda (k x y) (if (hash-has-key? monoids k)
                              ((monoid-join (hash-ref monoids k)) x y)
                              y)))])
    (make-env vals monoids)))

(define (env-single name val) (make-env (hash name val) (hash)))
(define (env-single-monoid name val) (make-env (hash) (hash name val)))
(define (env-from-list bindings)
  (reduce (map (lambda (x) (apply env-single x)) bindings)
    env-empty env-join))

(define (env-get env name)
  (hash-ref (env-vals env) name (lambda () (raise `(unbound ,name)))))

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
     map foldl foldr append
     string-append
     set set->list set-add set-member? set-remove set-union
     hash hash-has-key? hash-ref hash-remove hash-union
     ))

;; hacks to get eval to work as desired here
(namespace-require 'racket/set)
(namespace-require "util.rkt")
(define base-env
  (env-from-list (map (lambda (name) `(,name ,(racket:eval name))) prelude)))


;; Loading files
(define (run-file filename)
  (eval-body (read-file filename)))

;; REPL
(define (repl)
  (define (unbound? exn) (and (list? exn) (eq? 'unbound (car exn))))
  (define (handle-unbound exn)
    (printf "Reference to undefined variable: ~a\n" (cadr exn)))
  (let loop ([env base-env] [debug #f])
    (display "REPL: ")
    (match (read)
      [(or ':q ':quit) (error "Quitting.")]
      [':env
        (displayln "Current env is:")
        (print-env env)]
      [':reset (set! env base-env)]
      [':debug
        (set! debug (not debug))
        (printf "Debugging output is ~a\n" (if debug "on" "off"))]
      [decl
        (with-handlers ([unbound? handle-unbound])
          (let-values ([(val decl-env) (eval-decl decl env)])
            (when debug
              (displayln "Env changes:")
              (print-env decl-env))
            (unless (void? val) (println val))
            (set! env (env-join env decl-env))))])
    (loop env debug)))

(define (print-env env)
  (let ([vals (env-vals env)]
        [monoids (env-monoids env)])
    (let-values ([(procs vals) (partition (compose procedure? cdr)
                                 (hash->list vals))])
      (unless (null? procs)
        (printf "  Procedures: ~a\n"
          (string-join (sort (map (compose symbol->string car) procs) string<?)
            " ")))
      (printf "  Values: {~a}\n"
        (string-join
          (for/list ([v (sort vals symbol<? #:key car)])
            (format "~a: ~v" (car v) (cdr v)))
          ", "))
      (unless (hash-empty? monoids)
        (printf "  Monoids: ~a\n"
          (string-join (map symbol->string (hash-keys monoids)) " "))))))
