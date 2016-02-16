#lang racket
(define (enum-list my-list)
  (cond [(> (length my-list) 0)
       (write (first my-list))
       (enum-list (rest my-list))
       ])
  )

(define my-list (list (list 1 2) (list 1 3) (list 2 3) (list 3 3)))

(enum-list my-list)