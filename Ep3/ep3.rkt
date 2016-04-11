#lang racket

(define initial-state "S0")

(define function (make-hash))

(define function-S0 (make-hash))
(hash-set! function-S0 "0" "S0")
(hash-set! function-S0 "1" "S1")

(define function-S1 (make-hash))
(hash-set! function-S1 "0" "S1")
(hash-set! function-S1 "1" "S0")

(hash-set! function "S0" function-S0)
(hash-set! function "S1" function-S1)

(define end-states (list "S1"))

(define automata (list initial-state function end-states))

(define tape (list "1" "0" "1" "1" "0" "0" "1" "1"))

(define (get-next-state automata current-state element)
  (define function (second automata))
  (define state-function (hash-ref function current-state "NULL"))
  (cond [(equal? state-function "NULL")
         "NULL"
         ]
        [else
         (hash-ref state-function element "NULL")
         ])
  )


(define (run-tape automata tape current-state)
  (print current-state)
  (print tape)
  (display "\n")
  (cond [(> (length tape) 0)
         (define element (first tape))
         (define next-state (get-next-state automata current-state element))
         (cond [(equal? next-state "NULL")
                false
                ]
               [else
                (run-tape automata (rest tape) next-state)
                ])]
        [else
         (cond [(member current-state (third automata))
                true
                ]
               [else
                false
                ])
         ])
  )

(define (test-tape automata tape)
  (run-tape automata tape (first automata))
  )

(test-tape automata tape)

