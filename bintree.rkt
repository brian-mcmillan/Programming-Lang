; Auxiliary Functions 
; Purpose: Renaming car, cadr, caddr, integer?, for easier implementation.
; Creating constructors for interior node, leaf, and contents-of for ease of use
; in later function creation.

; The root node of the BST 
(define (get-root bt) (car bt))

; The left subtree of the BST
(define (get-left bt) (cadr bt))

; The right subtree of the BST
(define (get-right bt) (caddr bt))

;  Exercise 1.34
; 
; PATH
; ;Contract:
; (binary-tree -> list of strings)
; 
; ;Purpose:
; takes an integer n and a binary search tree bst
; that contains the integer n, and returns a list of lefts and
; rights showing how to find the node containing n. If
; n is found at the root, it returns
; the empty list.



(define (path n bt)
  (cond
    [ (= (get-root bt) n)'() ] 
    [ (< (get-root bt) n)(cons "right" (path n (get-right bt))) ]
    [(> (get-root bt) n)(cons "left" (path n (get-left bt))) ]
    ))


; Example:
; 
; n = 12
; 
; bst = '(14 (7 () (12 () ()))
;            (26 (20 (17 () ())
;                    ()) (31 () ())))
; 
; output : "left" "right"



; Test-Cases :

; binary search tree constructors

(define bst1 '(14 (7 () (12 () ()))
                  (26 (20 (17 () ())
                          ()) (31 () ()))))

(define bst2 '(42 (20 (12 () ())
                      (21 () (35 (25 () ()) (39 () ()))))
                  (100 () (211 () ()))))



; bst1
(check-expect(path 14 bst1) '())
(check-expect(path 7 bst1)  '("left"))
(check-expect(path 12 bst1) '("left" "right"))
(check-expect(path 26 bst1) '("right"))
(check-expect(path 20 bst1) '("right" "left"))
(check-expect(path 17 bst1) '("right" "left" "left"))
(check-expect(path 31 bst1) '("right" "right"))

;bst2
(check-expect(path 42 bst2) '())
(check-expect(path 20 bst2)  '("left"))
(check-expect(path 12 bst2) '("left" "left"))
(check-expect(path 21 bst2) '("left" "right"))
(check-expect(path 35 bst2) '("left" "right" "right"))
(check-expect(path 25 bst2) '("left" "right" "right" "left"))
(check-expect(path 39 bst2) '("left" "right" "right" "right"))
(check-expect(path 100 bst2) '("right"))
(check-expect(path 211 bst2) '("right" "right"))




; Exercise 1.35
; 
; number-leaves
; 
; ;Contract:
; bintree -> bintree
; ;Purpose:
; takes a bintree, and produces a bintree like the original,
; except the contents of the leaves are numbered
; starting from 0.
; ;EX input:
; (number-leaves
;  (interior-node 'foo
;                 (interior-node 'bar
;                                (leaf 26)
;                                (leaf 12))
;                 (interior-node 'baz
;                                (leaf 11)
;                                (interior-node 'quux
;                                               (leaf 117)
;                                               (leaf 14))
; ;EX output:
; (foo
;  (bar 0 1)
;  (baz
;   2
;   (quux 34)))


; Functions from textbook sample implementation
; Purpose: creates a leaf node
(define (leaf x) x)
; Purpose: create bt given root node and two child nodes
(define (interior-node n cadr caddr)
  (list n cadr caddr))
; Purpose: boolean to return node status as leaf
(define leaf? integer?)


; Additional functions
; get-contents
; Purpose: get contents of given bt
(define (get-contents bt)
  (if (leaf? bt) bt (car bt)))


; Main function
(define (number-leaves bt)

  ; helper
#|

What is the signature for helper?

|#
  ; purpose: mark accum (current leaf value) for current contents of bt
  (define (helper bt accum)

    ; if it is a leaf, replace it's value with n.
    ; accum + 1 so that the next leaf found is exactly one greater than the previous leaf.
    (if (leaf? bt)
#|

Where is the data definition for the returned value?

What is the accumulator invariant?

|#
        (cons (leaf accum) (+ accum 1))
        ; recursively move through the left-child (cadr) right-child (caddr)
        ; create new bt using get-contents
        (letrec ([lhs (helper (cadr bt) accum)] 
                 [rhs (helper (caddr bt) (cdr lhs))])
          (cons
           
           (interior-node (get-contents bt) 
                          (car lhs) 
                          (car rhs)) 
           (cdr rhs))))) ;
  (car (helper bt 0)))


; Testing for given book example
(define bt0 (interior-node 'foo
                           (interior-node 'bar
                                          (leaf 26)
                                          (leaf 12))
                           (interior-node 'baz
                                          (leaf 11)
                                          (interior-node 'quux
                                                         (leaf 117)
                                                         (leaf 14)))))

(define bt0-expected '(foo
                       (bar 0 1)
                       (baz
                        2
                        (quux 3 4))))
