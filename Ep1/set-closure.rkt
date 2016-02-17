#lang racket
(require "enum-list.rkt" "find-elements.rkt")

(define A (list 1 2 3 4))
(define R (list (list 1 2)(list 2 3) (list 3 3)))
(define resp (list))

(define (replace-start-point new-start some-pair)
  (cons new-start (cdr some-pair))
  )

(define (make-new-paths-from-list new-start target-list)
  (map (lambda (pair)
         (replace-start-point new-start pair))
       target-list)
  )

(define (right-element pair)
  (first (rest pair))
  )

(define added (list))
(define visited (list))

(define (apply-dfs target-list cur)
  (cond [(> (length target-list) 0) 
         (define new-element (right-element (first target-list)))
         (cond [(not (member new-element added))
                (set! added (append added (list new-element)))
                (set! resp (append resp (list (list cur new-element))))
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

(define (copy-list target-list source-list)
  (set! target-list (append target-list source-list))
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
  (copy-list resp R)
  (dfs-iterate A)
  resp
  )

(set-closure A R)