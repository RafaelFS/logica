#lang racket
(require "enum-list.rkt" "find-elements.rkt")

(define A (list 1 2 3 4 5))
(define R (list (list 1 2) (list 2 3) (list 3 1) (list 3 5) (list 4 5)))

(define resp (list))
(define visited (list))

(define (right-element pair)
  (first (rest pair))
  )

(define (apply-dfs target-list cur)
  (cond [(> (length target-list) 0) 
         (define new-element (right-element (first target-list)))
         (define vector (list cur new-element))
         (cond [(not (member vector resp))
                (set! resp (append resp (list vector)))
                ])
         (cond [(not (member new-element visited))
                (set! visited (append visited (list new-element)))
                (dfs new-element cur)
                ])
         (apply-dfs (rest target-list) cur)
  ])
)

(define (dfs v cur)
  (define found-elements (find-elements R v))
  (apply-dfs found-elements cur)
  )

(define (dfs-iterate A)
  (cond [(> (length A) 0)
         (define cur (first A))
         (define reflexive (list cur cur))
         (set! visited (list cur))
         (cond [(not (member reflexive resp))
                (set! resp (append resp (list reflexive)))
                ])
         (dfs cur cur)
         (dfs-iterate (rest A))
         ])
)

(define (set-closure A R)
  (set! resp R)
  (dfs-iterate A)
  resp
  )

(display "A:\n")
(display A)

(display "\n\nR:\n")
(display R)

(display "\n\nClosure:\n")
(display (set-closure A R))