(module a
  (define in-a "a")
  (define in-both "a"))

(module b
  (define in-b "b")
  (define in-both "b"))

(import a)
(import b)
