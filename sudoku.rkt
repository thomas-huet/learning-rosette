#lang rosette

(define (digit? n)
  (and (integer? n) (>= n 1) (<= n 9)))

(define (valid-block block)
  (and (andmap digit? block) (apply distinct? block)))

(define (transpose grid)
  (if (null? (car grid))
    '()
    (cons (map car grid) (transpose (map cdr grid)))))

(define (cells grid)
  (define (three l) (list (car l) (car (cdr l)) (car (cdr (cdr l)))))
  (define (tail l) (cdr (cdr (cdr l))))
  (define (aux l)
    (if (null? (car l))
      '()
      (cons (apply append (map three l)) (aux (map tail l)))))
  (if (null? grid)
    '()
    (append (aux (three grid)) (cells (tail grid)))))

(define (valid grid)
  (and
    (andmap valid-block grid)
    (andmap valid-block (transpose grid))
    (andmap valid-block (cells grid))))

(define (solve-sudoku incomplete)
  (define (new-symbol)
    (define-symbolic* x integer?)
    x)
  (define (place-symbol n)
    (if (digit? n)
      n
      (new-symbol)))
  (define grid (map (lambda (l) (map place-symbol l)) incomplete))
  (define sol (solve (assert (valid grid))))
  (map (lambda (l) (map (lambda (x) (evaluate x sol)) l)) grid))

(define a
`((x 4 9 3 2 x 8 x x)
  (x x x x 1 7 x x 4)
  (7 x x x x x 6 2 x)
  (6 x x x 5 x x x x)
  (9 x x x 7 x x x 8)
  (x x x x 3 x x x 5)
  (x 8 1 x x x x x 3)
  (3 x x 2 4 x x x x)
  (x x 5 x 9 3 2 7 x)))

(solve-sudoku a)
