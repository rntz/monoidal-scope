#lang racket

(provide
  repr
  const fix flip partial nary unary
  zip-with zip map_
  matches? lambda-rec eta
  foldl1 reduce dict-union
  println read-file)

(define (repr x) (with-output-to-string (lambda () (write x))))

(define (const x) (lambda _ x))
(define (fix f) (lambda args (apply f f args)))

(define ((partial f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))

(define ((flip f) x y) (f y x))

;; zips arbitrary sequences, not just lists
(define (zip-with f xs ys)
  (for/list ([x xs] [y ys]) (f x y)))

(define (zip xs ys) (zip-with list xs ys))

(define (map_ f . xs) (apply map f xs) (void))


;;; Some syntactic help
(define-syntax-rule (matches? exp pat)
  (match exp [pat #t] [_ #f]))

(define-syntax-rule (lambda-rec name rest ...)
  (letrec ((name (lambda rest ...))) name))

(define-syntax-rule (eta f) (lambda x (apply f x)))


;; Data structure manipulations
;; not sure I need this
(define (foldl1 f xs)
  (match xs
    ['() (error "foldl1 called on empty list")]
    [`(,x) x]
    [(cons x xs) (foldl f x xs)]))

;; Reduces a list using a monoid
(define (reduce list identity function)
  (match list
    ['() identity]
    [`(,a) a]
    [`(,a ,b) (function a b)]
    [(cons a as) (foldl (flip function) a as)]))

(define (dict-union a b [combine (lambda (k x y) y)])
  (cond
    [(dict-empty? a) b]
    [(dict-empty? b) a]
    [else
      ;; TODO: this is inefficient
      (dict-for-each b
        (lambda (key val)
          (set! a (dict-set a key
                    (if (dict-has-key? a key)
                     (combine key (dict-ref a key) val)
                      val)))))
      a]))


;; I/O procedures
(define (println x) (print x) (display "\n"))

;; Reads all s-expressions from filename
(define (read-file path)
  (with-input-from-file path
    (lambda ()
      (let loop ([sexps '()])
        (let ([x (read)])
          (if (eof-object? x) (reverse sexps)
            (loop (cons x sexps))))))))
