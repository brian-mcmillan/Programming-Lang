#lang eopl

; Brian McMillan, Nicholas Gannon, Eric Batzler
; 5.29 - Programming Languages - Dec 7th, 2022

; [*] Apply the transformation of this section to fact-iter
; (page 139)

; --- 139 --- *sample code*
(require test-engine/racket-tests)

; ; fact
; (define fact
;   (lambda (n)
;     (if (zero? n) 1 (* n (fact (- n 1 ))))))
; ; fact-iter
; (define fact-iter
;   (lambda (n)
;     (fact-iter-acc n 1)))
; ; fact-iter-acc
; (define fact-iter-acc
;   (lambda (n a)
;     (if (zero? n) a (fact-iter-acc (-n 1) (* n a )))))
; 


; global scope var
(define n 'uninitialized) ; the number to be manipulated
(define a 'uninitialized) ; the accumulator


; (NEW) fact-iter-acc
; **purpose -> helper function for fact-iter
; **finds factorial of n--where the accum is a.
(define (fact-iter-acc)
    (if (zero? n) a
         (begin
           (set! a (* n a))
           (set! n (- n 1))
           (fact-iter-acc))))

; (NEW) fact-iter
; **purpose -> finds fact! of n
; n = num, acc = accum
(define (fact-iter)
  (set! a 1)
  (fact-iter-acc))

(check-expect (begin (set! n 0)
                     (fact-iter)) 1)
(check-expect (begin (set! n 1)
                     (fact-iter)) 1) 
(check-expect (begin (set! n 4)
                     (fact-iter)) 24)
(check-expect (begin (set! n 5)
                     (fact-iter)) 120)
(check-expect (begin (set! n 8)
                     (fact-iter)) 40320)
(check-expect (begin (set! n 10)
                     (fact-iter)) 3628800)
(check-expect (begin (set! n 13)
                     (fact-iter)) 6227020800)
(check-expect (begin (set! n 2)
                     (fact-iter)) 2)


(test)
