#lang racket

(provide find-elements)

(define (find-elements my-list x)
  (define elements-list (list))
  (cond [(> (length my-list) 0)
       (cond [(equal? (first (first my-list)) x)
              (set! elements-list (list (first my-list)))
              ])
       (set! elements-list (append elements-list (find-elements (rest my-list) x)))
       ])
  elements-list
  )

(define my-list (list (list 1 2) (list 1 3) (list 2 3) (list 3 3)))