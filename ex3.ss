(monoidify names set-union (set))

(define names (set "ada" "church"))

(define (get-names) names)
(define (with-names x)
  (define names x) ;; defines a "local variable"?????
  names)
