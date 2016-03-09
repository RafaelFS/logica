#lang racket

(define w (list "a" "a" "a" "b" "b" "b" "c" "c" "c"))

(define V (list "a" "b" "c" "S" "B" "C"))

(define E (list "a" "b" "c"))

(define R (list
             (list (list "S") (list "a" "B" "C"))
             (list (list "S") (list "a" "S" "B" "C"))
             (list (list "a" "B") (list "a" "b"))
             (list (list "b" "B") (list "b" "b"))
             (list (list "b" "C") (list "b" "c"))
             (list (list "c" "C") (list "c" "c"))
             (list (list "C" "B") (list "B" "C"))
             ))

(define S (list "S"))

(define G (list V E R S)) 

(define (apply-rule index alpha alphai betai Dcur NovasCur w)
  (cond [(<= index (- (length alpha) (length alphai)))
         (define x (take alpha index))
         (define aux (drop alpha index))
         (define test (take aux (length alphai)))
         (define y (drop aux (length alphai)))
         (define nova (append x (append betai y)))
         (cond [(and (equal? test alphai)
                     (not (member nova Dcur))
                     (not (member nova NovasCur))
                     (<= (length nova)(length w)))
                (set! NovasCur (append NovasCur (list nova)))])
         (apply-rule (+ index 1) alpha alphai betai Dcur NovasCur w)]
        [else NovasCur]))

(define (iterateR R alpha Dcur NovasCur w)
  (cond [(> (length R) 0)
         (define Ri (first R))
         (define alphai (first Ri))
         (define betai (second Ri))
         (set! NovasCur (apply-rule 0 alpha alphai betai Dcur NovasCur w))
         (iterateR (rest R) alpha Dcur NovasCur w)]
        [else NovasCur])
  )

(define (iterateNovas R Dcur NovasPrev NovasCur w)
  (cond [(> (length NovasPrev) 0)
         (define alpha (first NovasPrev))
         (set! NovasCur (iterateR R alpha Dcur NovasCur w))
         (iterateNovas R Dcur (rest NovasPrev) NovasCur w)]
        [else NovasCur])
  )

(define (find-sentences R Dcur NovasPrev w)
  (cond [(> (length NovasPrev) 0)
         (define NovasCur (iterateNovas R Dcur NovasPrev (list) w))
         (set! Dcur (append Dcur NovasCur))
         (find-sentences R Dcur NovasCur w)]
        [else Dcur]))

(define (main G w)
  (define R (third G))
  (define S (fourth G))
  (define all-sentences (find-sentences R (list S) (list S) w))
  (cond [(member w all-sentences) true]
        [else false]))

(main G w)
  