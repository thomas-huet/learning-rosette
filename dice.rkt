#lang rosette/safe

(define (wins-against? a b)
  (define (wins a b)
    (define (aux a b)
      (if (null? b)
        0
        (if (> a (car b))
          (+ 1 (aux a (cdr b)))
          (aux a (cdr b)))))
    (if (null? a)
      0
      (+ (aux (car a) b) (wins (cdr a) b))))
  (> (wins a b) (wins b a)))

(define (non-transitive? a b c)
  (and (wins-against? a b) (wins-against? b c) (wins-against? c a)))

(define (symbolic-die n)
  (define (new-symbol)
    (define-symbolic* x integer?)
    (assert (positive? x))
    x)
  (if (= n 0)
    '()
    (cons (new-symbol) (symbolic-die (- n 1)))))

(define (find-non-transitive-dice n)
  (define a (symbolic-die n))
  (define b (symbolic-die n))
  (define c (symbolic-die n))
  (define sol (solve (assert (non-transitive? a b c))))
  (evaluate (list a b c) sol))

(find-non-transitive-dice 7)
