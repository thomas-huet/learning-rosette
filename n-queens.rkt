#lang rosette/safe

(define (id x) x)

(define (exactly-one? l)
  (= (length (filter id l)) 1))

(define (at-most-one? l)
  (<= (length (filter id l)) 1))

(define (transpose board)
  (if (null? (car board))
    '()
    (cons (map car board) (transpose (map cdr board)))))

(define (diagonals board)
  (define (aux n board)
    (if (or (= n 0) (null? board))
      (cons '() board)
      (let ([tail (aux (- n 1) (cdr board))])
        (cons (cons (caar board) (car tail)) (cons (cdar board) (cdr tail))))))
  (define (diag n board)
    (if (null? board)
      '()
      (if (null? (car board))
        (diag (- n 1) (cdr board))
        (let ([a (aux n board)])
          (cons (car a) (diag (+ n 1) (cdr a)))))))
  (diag 1 board))

(define (valid? board)
  (and
    (andmap exactly-one? board)
    (andmap exactly-one? (transpose board))
    (andmap at-most-one? (diagonals board))
    (andmap at-most-one? (diagonals (reverse board)))))

(define (symbolic-board n)
  (define (new-symbol)
    (define-symbolic* x boolean?)
    x)
  (define (row n)
    (if (= n 0)
      '()
      (cons (new-symbol) (row (- n 1)))))
  (define (board m)
    (if (= m 0)
      '()
      (cons (row n) (board (- m 1)))))
  (board n))

(define (queens n)
  (define board (symbolic-board n))
  (define sol (solve (assert (valid? board))))
  (evaluate board sol))

(queens 8)
