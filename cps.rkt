
(define COLOR 'green)

;; Sample line images
(define T1 (line 200 0 COLOR))
(define T2 (line 600 0 COLOR))

(define (draw-koch-curve line-img)
  (local [(define len (image-width line-img))]
    (if (<= len 11)
        line-img
        (beside/align 'bottom
                      (draw-koch-curve (line (* 1/3 len) 0 COLOR))
                      (rotate 60 (draw-koch-curve (line (* 1/3 len) 0 COLOR)))
                      (rotate -60 (draw-koch-curve (line (* 1/3 len) 0 COLOR)))
                      (draw-koch-curve (line (* 1/3 len) 0 COLOR))))))

(define (draw-koch-curve/k line-img k)
  (local [(define len (image-width line-img))]
    (if (<= len 11)
        (k line-img)
        (beside/align 'bottom
                      (draw-koch-curve/k (line (* 1/3 len) 0 COLOR) (lambda (a) (k a)))
                      (rotate 60 (draw-koch-curve/k (line (* 1/3 len) 0 COLOR) (lambda (a) (k a))))
                      (rotate -60 (draw-koch-curve/k (line (* 1/3 len) 0 COLOR) (lambda (a) (k a))))
                      (draw-koch-curve/k (line (* 1/3 len) 0 COLOR) (lambda (a) (k a)))))))

; **endk not working--try (lambda

(check-expect (draw-koch-curve/k T1 (lambda (a) a))
              .)
(check-expect (draw-koch-curve/k T2 (lambda (a) a))
              .)


(define (ackermann m n)
  (cond ((= m 0) (add1 n))       
        ((= n 0) (ackermann (sub1 m) 1))     
        (else (ackermann (sub1 m)
                         (ackermann m (sub1 n))))))

(define (ackermann/k m n k)
  (cond [(= m 0) (k (add1 n))]
        [(= n 0) (ackermann/k (sub1 m) 1 (lambda (a) (k a)))]
        [else (ackermann/k (sub1 m) (ackermann m (sub1 n)) (lambda (a) (k a)))]))

(check-expect (ackermann/k 1 2 (lambda (a) a)) 4)
(check-expect (ackermann/k 2 1 (lambda (a) a)) 5)
(check-expect (ackermann/k 2 3 (lambda (a) a)) 9)
(check-expect (ackermann/k 3 2 (lambda (a) a)) 29)
(check-expect (ackermann/k 3 3 (lambda (a) a)) 61)
(check-expect (ackermann/k 3 4 (lambda (a) a)) 125)
