#lang racket

(define initial-state "S")

(define function (make-hash))

(define function-S (make-hash))
(hash-set! function-S "a" (list "S1"))
(hash-set! function-S "b" (list "S2"))
(hash-set! function-S "" (list "H" "B"))

(define function-S1 (make-hash))
(hash-set! function-S1 "b" (list "A"))

(define function-S2 (make-hash))
(hash-set! function-S2 "a" (list "B"))

(define function-A (make-hash))
(hash-set! function-A "b" (list "S"))

(define function-B (make-hash))
(hash-set! function-B "a" (list "S"))

(define function-H (make-hash))

(hash-set! function "S" function-S)
(hash-set! function "S1" function-S1)
(hash-set! function "S2" function-S2)
(hash-set! function "A" function-A)
(hash-set! function "B" function-B)
(hash-set! function "H" function-H)

(define end-states (list "A" "H"))

(define nd-automata (list initial-state function end-states))

(define tape (list "a" "b" "b"))

(define (get-next-states nd-automata current-state element)
  (define function (second nd-automata))
  (define state-function (hash-ref function current-state (make-hash)))
  (hash-ref state-function element (list))
  )

(define (copy-next-states nd-automata tape next-states)
  (cond [(equal? (length next-states) 0)
         false
         ]
        [(equal? (length next-states) 1)
         (run-tape nd-automata tape (first next-states))
         ]
        [else
         (or
          (run-tape nd-automata tape (first next-states))
          (copy-next-states nd-automata tape (rest next-states)))
        ])
  )

(define (run-tape nd-automata tape current-state)
  (print current-state)
  (print tape)
  (display "\n")
  (define empty-next-states (get-next-states nd-automata current-state ""))
  (or
   (copy-next-states nd-automata tape empty-next-states)
   (cond [(> (length tape) 0)
         (define element (first tape))
         (define next-states (get-next-states nd-automata current-state element))
         (copy-next-states nd-automata (rest tape) next-states)
         ]
        [else
         (cond [(member current-state (third nd-automata))
                true
                ]
               [else
                false
                ])
         ]))
  )

(define (test-tape nd-automata tape)
  (run-tape nd-automata tape (first nd-automata))
  )

(test-tape nd-automata tape)

