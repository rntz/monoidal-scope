2
(+ 2 3)
(display "hello, hacker school\n")

(define x 2)
x

(define x "a string")
x

(define (id x) x) ;; identity function
(id "unchanged")

(define (double x)
  (define y (+ x x)) ;; defines a local variable
  z)
(double 2) ;; returns 4
y ;; undefined

:reset
(module a (define x 2))
x
a
(import a)
x


;; Show ex2.ss
:reset
(use "ex2.ss")
(import a)
(import b)
in-a
in-b
in-both ;; acts like Python

:reset
(monoidify in-both string-append "")
(use "ex2.ss")
(import a)
(import b)
in-a
in-b
in-both ;; WOAH!

;; explain monoidify
(define in-both "xyzzy")
in-both


;; Quick aside on scope
:reset
(define x "original")
(define (get-x) x)
(get-x)

(define x "changed")
(get-x)

;; get-x uses the value of x at the point get-x was defined.
;;
;; (define x) isn't mutating x in-place, it's updating the environment (which
;; maps vars to values) for future expressions.

;; ;; let x = "original"
;; ;;     f () = x ;; remembers value of x
;; ;; in let x = "changed"
;; ;;    in f ()


;; Show ex3.ss
;; explain set, set-union
:reset
(use "ex3.ss")
names
(get-names)
;; guess the following:
(with-names (set "hopper")) ;; woah!
names ;; scope!

(define names (set "liskov"))
names
;; guesses?
(get-names) ;; a-ha! it uses the value of names at its point of definition!
(with-names (set "mccarthy"))

;; names acts like a *scoped set of names*.
;; importing a module can bring changes to this set into scope.
(module a (define names (set "turing")))
(import a)
names

;; That's all.
