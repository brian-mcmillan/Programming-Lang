#lang eopl
(require test-engine/racket-tests)

; 4.20
;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    ;;
    (expression
     ("letmutable" identifier "=" expression "in" expression)
     letmutable-exp)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   
    
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    (expression
     ("letrec"
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)
    
    ;; new for explicit-refs
    
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)
    
    (expression
     ("newref" "(" expression ")")
     newref-exp)
    
    (expression
     ("deref" "(" expression ")")
     deref-exp)
    
    (expression
     ("setref" "(" expression "," expression ")")
     setref-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define (show-the-datatypes) 
  (sllgen:list-define-datatypes the-lexical-spec the-grammar))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (ref-val
   (ref reference?))
  )

;;; extractors:

(define (expval->num v)
  (cases expval v
    (num-val (num) num)
    (else (expval-extractor-error 'num v))))

(define (expval->bool v)
  (cases expval v
    (bool-val (bool) bool)
    (else (expval-extractor-error 'bool v))))

(define (expval->proc v)
  (cases expval v
    (proc-val (proc) proc)
    (else (expval-extractor-error 'proc v))))

(define (expval->ref v)
  (cases expval v
    (ref-val (ref) ref)
    (else (expval-extractor-error 'reference v))))

(define (expval-extractor-error variant value)
  (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
              variant value))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;(define-datatype denval denval?)

(define-datatype environment environment?
  
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   ;denval
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))

;; env->list : Env -> List
;; used for pretty-printing and debugging
(define (env->list env)
  (cases environment env
    (empty-env () '())
    (extend-env (sym val saved-env)
                (cons
                 (list sym (expval->printable val))
                 (env->list saved-env)))
    (extend-env-rec* (p-names b-vars p-bodies saved-env)
                     (cons
                      (list 'letrec p-names '...)
                      (env->list saved-env)))))

;; expval->printable : ExpVal -> List
;; returns a value like its argument, except procedures get cleaned
;; up with env->list 
(define (expval->printable val)
  (cases expval val
    (proc-val (p)
              (cases proc p
                (procedure (var body saved-env)
                           (list 'procedure var '... (env->list saved-env)))))
    (else val)))


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;


;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69
(define (init-env)
  (extend-env 
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "No binding for ~s" search-sym))
    (extend-env (bvar bval saved-env)
                (if (eqv? search-sym bvar)
                    bval
                    (apply-env saved-env search-sym)))
    (extend-env-rec* (p-names b-vars p-bodies saved-env)
                     (cond 
                       ((location search-sym p-names)
                        => (lambda (n)
                             (proc-val
                              (procedure 
                               (list-ref b-vars n)
                               (list-ref p-bodies n)
                               env))))
                       (else (apply-env saved-env search-sym))))))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define (location sym syms)
  (cond
    ((null? syms) #f)
    ((eqv? sym (car syms)) 0)
    ((location sym (cdr syms))
     => (lambda (n) 
          (+ n 1)))
    (else #f)))


;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

;;; world's dumbest model of the store:  the store is a list and a
;;; reference is number which denotes a position in the list.

;; the-store: a Scheme variable containing the current state of the
;; store.  Initially set to a dummy variable.
(define the-store 'uninitialized)

;; empty-store : () -> Sto
(define (empty-store) '())

;; initialize-store! : () -> Sto
;; usage: (initialize-store!) sets the-store to the empty-store
(define (initialize-store!)
  (set! the-store (empty-store)))

;; get-store : () -> Sto
;; This is obsolete.  Replaced by get-store-as-list below
(define (get-store) the-store)

;; reference? : SchemeVal -> Bool
(define (reference? v)
  (integer? v))

;; newref : ExpVal -> Ref
(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store
          (append the-store (list val)))                    
    next-ref))                     

;; deref : Ref -> ExpVal
(define (deref ref)
  (list-ref the-store ref))

;; setref! : Ref * ExpVal -> Unspecified
(define (setref! ref val)
  (set! the-store
        (letrec
            ((setref-inner
              ;; returns a list like store1, except that position ref1
              ;; contains val. 
              (lambda (store1 ref1)
                (cond
                  ((null? store1)
                   (report-invalid-reference ref the-store))
                  ((zero? ref1)
                   (cons val (cdr store1)))
                  (else
                   (cons
                    (car store1)
                    (setref-inner
                     (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref))))

(define (report-invalid-reference ref the-store)
  (eopl:error 'setref
              "illegal reference ~s in store ~s"
              ref the-store))

;; get-store-as-list : () -> Listof(List(Ref,Expval))
;; Exports the current state of the store as a scheme list.
;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
;;   where foo, bar, and baz are expvals.
;; If the store were represented in a different way, this would be
;; replaced by something cleverer.
(define (get-store-as-list)
  (letrec
      ((inner-loop
        ;; convert sto to list as if its car was location n
        (lambda (sto n)
          (if (null? sto)
              '()
              (cons
               (list n (car sto))
               (inner-loop (cdr sto) (+ n 1)))))))
    (inner-loop the-store 0)))



;;;; Interpreter


;; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (initialize-store!)               ; new for explicit refs.
  (cases program pgm
    (a-program (exp1)
               (value-of exp1 (init-env)))))

;; value-of : Exp * Env -> ExpVal
(define (value-of exp env)
  (cases expression exp
    
    (const-exp (num) (num-val num))
    
    (var-exp (var) (apply-env env var))


             ;(if (reference? (apply-env env var))
             
              ;(deref (apply-env ??

     (letmutable-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var (newref val1) env))))
    
    (diff-exp (exp1 exp2)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                (let ((num1 (expval->num val1))
                      (num2 (expval->num val2)))
                  (num-val
                   (- num1 num2)))))
    
    (zero?-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((num1 (expval->num val1)))
                   (if (zero? num1)
                       (bool-val #t)
                       (bool-val #f)))))
    
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (value-of exp1 env)))
              (if (expval->bool val1)
                  (value-of exp2 env)
                  (value-of exp3 env))))
    
    (let-exp (var exp1 body)       
             (let ((val1 (value-of exp1 env)))
               (value-of body
                         (extend-env var val1 env))))
    
    (proc-exp (var body)
              (proc-val (procedure var body env)))
    
    (call-exp (rator rand)
              (let ((proc (expval->proc (value-of rator env)))
                    (arg (value-of rand env)))
                (apply-procedure proc arg)))
    
    (letrec-exp (p-names b-vars p-bodies letrec-body)
                (value-of letrec-body
                          (extend-env-rec* p-names b-vars p-bodies env)))
    
    (begin-exp (exp1 exps)
               (letrec 
                   ((value-of-begins
                     (lambda (e1 es)
                       (let ((v1 (value-of e1 env)))
                         (if (null? es)
                             v1
                             (value-of-begins (car es) (cdr es)))))))
                 (value-of-begins exp1 exps)))
    
    (newref-exp (exp1)
                (let ((v1 (value-of exp1 env)))
                  (ref-val (newref v1))))
    
    (deref-exp (exp1)
               (let ((v1 (value-of exp1 env)))
                 (let ((ref1 (expval->ref v1)))
                   (deref ref1))))
    
    (setref-exp (exp1 exp2)
                (let ((ref (expval->ref (value-of exp1 env))))
                  (let ((v2 (value-of exp2 env)))
                    (begin
                      (setref! ref v2)
                      (num-val 23)))))
    ))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define (apply-procedure proc1 arg)
  (cases proc proc1
    (procedure (var body saved-env)
               (let ((r arg))
                 (let ((new-env (extend-env var r saved-env)))
                   (value-of body new-env))))))


;; store->readable : Listof(List(Ref,Expval)) 
;;                    -> Listof(List(Ref,Something-Readable))
(define (store->readable l)
  (map
   (lambda (p)
     (cons
      (car p)
      (expval->printable (cadr p))))
   l))


;;; TOP LEVEL ;;;

;; eval : String -> ExpVal

(define (eval string)
  (value-of-program
   (scan&parse string)))

;;; EXAMPLES

; (eval "if zero?(1) then 1 else 2")
; (eval "-(x, v)")
; (eval "if zero?(-(x, x)) then x else 2")
; (eval "if zero?(-(x, v)) then x else 2")
; (eval "let decr = proc (a) -(a, 1) in (decr 30)")
; (eval "( proc (g) (g 30) proc (y) -(y, 1))")
; (eval "let x = 200 
;          in let f = proc (z) -(z, x) 
;               in let x = 100 
;                    in let g = proc (z) -(z, x) 
;                         in -((f 1), (g 1))")
; (eval "let t = newref(3)
;        in deref(t)")
; 
; (eval "let t = newref(3)
;        in begin
;             setref(t, 10);
;             deref(t)
;           end")
; 
; 
; 
; (eval "let temp = newref(0)
;        in  let x = newref(5)
;            in  let mystery = proc (y)
;                                begin
;                                  setref(temp, x); 
;                                  setref(x, deref(y));
;                                  setref(y, deref(temp));
;                                  deref(x)
;                                end
;                in (mystery newref(10))")
; 
; (eval "let g = let counter = newref(0)
;                in proc (dummy)
;                  begin
;                   setref(counter, -(deref(counter), -1));
;                   deref(counter)
;                  end
;        in let a = (g 11)
;           in let b = (g 11)
;              in -(a, b)")
; 



(test)
