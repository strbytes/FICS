#lang htdp/isl+
; #lang racket


(define op first)
(define left second)
(define right third)
 
; Naive implementation

; (define (eval base)
;   (cond
;     [(number? base) base]
;     [(list? base) (f-apply (op base)
;                          (eval (left base))
;                          (eval (right base)))]))
 
(define (f-apply op val1 val2)
  (cond
    [(symbol=? op '+) (+ val1 val2)]
    [(symbol=? op '*) (* val1 val2)]))

; Use a struct to store binary operations

(define-struct bin (op fst snd))

; ; parse: sexp -> AST
 
; (define (parse sx)
;   (cond
;     [(and (list? sx) (equal? (op sx) '+))
;        (make-bin '+ (parse (left sx)) (parse (right sx)))]
;     [(and (list? sx) (equal? (op sx) '*))
;        (make-bin '* (parse (left sx)) (parse (right sx)))]
;     [else sx]))

; ; eval: AST -> number
 
; (define (eval ast)
;   (cond
;     [(and (bin? ast) (equal? (bin-op ast) '+))
;        (+ (eval (bin-fst ast)) (eval (bin-snd ast)))]
;     [(and (bin? ast) (equal? (bin-op ast) '*))
;        (+ (eval (bin-fst ast)) (eval (bin-snd ast)))]
;     [else ast]))


(define (reducible? ast) (not (number? ast)))
 
; (define (one-step ast)
;   (cond
;     [(and (number? (bin-fst ast)) (number? (bin-snd ast)))
;        (f-apply (bin-op ast) (bin-fst ast) (bin-snd ast))]
;     [(number? (bin-fst ast))
;        (make-bin (bin-op ast) (bin-fst ast) (one-step (bin-snd ast)))]
;     [else
;        (make-bin (bin-op ast) (one-step (bin-fst ast)) (bin-snd ast))]))
 
; (define (step-eval ast)
;   (cond
;     [(reducible? ast) (step-eval (one-step ast))]
;     [else ast]))

; Exercise 53

; 16
; 16

; Exercise 54

(define-struct f-let (names expr))
(define-struct pair (name val))

; ; parse: sexp -> AST
; ; Purpose: Parse an s-expression to an AST. Supports * + and let.
; ; Examples
; (check-expect (parse '(let ([a 1] [b 2]) (+ a b)))
;     (make-f-let (list (make-pair 'a 1) (make-pair 'b 2)) (make-bin '+ 'a 'b)))

; (define (parse sx)
;   (cond
;     [(and (list? sx) (equal? (op sx) 'let))
;         (parse-let (left sx) (right sx))]
;     [(and (list? sx) (equal? (op sx) '+))
;        (make-bin '+ (parse (left sx)) (parse (right sx)))]
;     [(and (list? sx) (equal? (op sx) '*))
;        (make-bin '* (parse (left sx)) (parse (right sx)))]
;     [else sx]))

; parse-let: sexp -> AST
; Purpose: Helper function for parse. Returns the AST of a let expression.
;   Takes a list of name-value mappings as s-expressions as lft and an s-expression as rgt.
; Examples:
(check-expect (parse-let '([a 1] [b 2]) '(+ a b))
    (make-f-let (list (make-pair 'a 1) (make-pair 'b 2)) (make-bin '+ 'a 'b)))

(define (parse-let lft rgt)
    (make-f-let (parse-pairs lft) (parse rgt)))

; parse-pairs: sexp -> (Listof pairs)
; Purpose: Helper function for parse-let. 
;   Takes a list of name-value mappings as s-expressions. Builds a list of pairs.
; Examples:
(check-expect (parse-pairs '([a 1] [b 2])) (list (make-pair 'a 1) (make-pair 'b 2)))

(define (parse-pairs pr)
    (cond
        [(empty? pr) empty]
        [else (cons (make-pair (first (first pr)) (parse (second (first pr)) )) 
            (parse-pairs (rest pr)))]))

; ; eval: AST -> number
; ; Purpose: Evaluate a faux-racket AST
; ; Example:
; (check-expect (eval (parse '(let ([a 1] [b 2]) (+ a b)))) 3)
 
; (define (eval ast)
;   (cond
;     [(f-let? ast) (eval (do-let-form (f-let-names ast) (f-let-expr ast)))]
;     [(and (bin? ast) (equal? (bin-op ast) '+))
;        (+ (eval (bin-fst ast)) (eval (bin-snd ast)))]
;     [(and (bin? ast) (equal? (bin-op ast) '*))
;        (* (eval (bin-fst ast)) (eval (bin-snd ast)))]
;     [else ast]))

; let-eval: (Listof pairs) AST -> AST
; Purpose: Perform all substitutions of the name-value mappings in the list names
;   in the AST expr.

(define (do-let-form names expr)
    (cond
        [(empty? names) expr]
        [else
            (do-let-form (rest names) (do-let-once (first names) expr))]))

; let-eval-once: pair AST -> AST
; Purpose: Perform a single substitution of a name-value mapping in the AST expr.

(define (do-let-once name expr)
    (cond
        [(f-let? expr)
            (do-let-once name (do-let-form (f-let-names expr) (f-let-expr expr)))]
        [(bin? expr)
            (make-bin
                (bin-op expr) 
                (cond
                    [(number? (bin-fst expr)) (bin-fst expr)]
                    [(equal? (bin-fst expr) (pair-name name))
                        (pair-val name)]
                    [else (do-let-once name (bin-fst expr))])
                (cond
                    [(number? (bin-snd expr)) (bin-snd expr)]
                    [(equal? (bin-snd expr) (pair-name name))
                        (pair-val name)]
                    [else (do-let-once name (bin-snd expr))]))]
        [else expr]))

; Exercise 55

(define (one-step ast)
  (cond
    [(f-let? ast) (step-eval (step-let-form (f-let-names ast) (f-let-expr ast)))]
    [(and (number? (bin-fst ast)) (number? (bin-snd ast)))
       (f-apply (bin-op ast) (bin-fst ast) (bin-snd ast))]
    [(number? (bin-fst ast))
       (make-bin (bin-op ast) (bin-fst ast) (one-step (bin-snd ast)))]
    [else
       (make-bin (bin-op ast) (one-step (bin-fst ast)) (bin-snd ast))]))
 
(define (step-eval ast)
  (cond
    [(reducible? ast) (step-eval (one-step ast))]
    [else ast]))

; let-reducible: AST Symbol -> Boolean
; Purpose: Returns whether an AST can be reduced by let-step.

; (define (let-reducible? ast name) 
;     (or (not (equal? ast name)) (not (number? ast)) (not (symbol? ast))))

; let-step: AST symbol AST -> AST
; Purpose: Performs a single step of substitution of name for expr in ast.
; Example:
(check-expect (let-step (make-pair 'a 1) (parse '(+ a a))) (make-bin '+ 1 1))

(define (let-step name expr)
    (cond
        [(equal? (pair-name name) expr) (pair-val name)]
        [(f-let? expr) (let-step name (step-let-form (f-let-names expr) (f-let-expr expr)))]
        [(bin? expr)
            (make-bin (bin-op expr) 
            (let-step name (bin-fst expr))
            (let-step name (bin-snd expr)))]
        [else expr]))

; step-let-form: (Listof pairs) AST -> AST
; Purpose: Apply let-step to all names from a let expression.
; Example:
(check-expect (step-let-form 
    (f-let-names (parse '(let ([a 1] [b 2]) (let ([c 3]) (* (+ a b) c)))))
    (f-let-expr (parse '(let ([a 1] [b 2]) (let ([c 3]) (* (+ a b) c))))))
    (make-bin '* (make-bin '+ 1 2) 3))

(define (step-let-form names expr)
    (cond
        [(empty? names) expr]
        [else
            (step-let-form (rest names) (let-step (first names) expr))]))

; Exercise 56

; parse: sexp -> AST
; Purpose: Parse an s-expression to an AST. Supports * + and let.
; Examples
(check-expect (parse '(let ([a 1] [b 2]) (+ a b)))
    (make-f-let (list (make-pair 'a 1) (make-pair 'b 2)) (make-bin '+ 'a 'b)))

(define (parse sx)
  (cond
    [(and (list? sx) (equal? (first sx) 'let))
        (parse-let (second sx) (third sx))]
    [(and (list? sx) (symbol? (op sx)))
       (make-bin (op sx) (parse (left sx)) (parse (right sx)))]
    [else sx]))

(define-struct def (name params expr))

; parse-def-list: (Listof sexp) (Listof def)
; Purpose: Parse a list of define statements into a list of def structs.
;   If a name is duplicated, the last definition with that name will be used.
(check-expect (parse-def-list '((define (a) (+ 1 2)) (define (a b c) (* b c))) empty)
    (list (make-def 'a '(b c) (make-bin '* 'b 'c)) (make-def 'a '() (make-bin '+ 1 2))))

(define (parse-def-list sx-list def-list)
    (cond
        [(empty? sx-list) def-list]
        [else (parse-def-list 
            (rest sx-list) 
            (cons (parse-def (first sx-list)) def-list))]))

(define (parse-def sx)
    (cond
        [(and (list? sx) (equal? (first sx) 'define))
            (let (  [name (first (first (rest sx)))]
                    [params (rest (first (rest sx)))]
                    [expr (parse (first (rest (rest sx))))])
                (make-def name params expr))]
        [else (error "Expected a define statement")]))

; eval: AST -> number
; Purpose: Evaluate a faux-racket AST
; Example:
; (check-expect (eval (parse '(let ([a 1] [b 2]) (+ a b)))) 3)
 
(define (eval ast defs)
  (cond
    [(f-let? ast) (eval (do-let-form (f-let-names ast) (f-let-expr ast)))]
    [(bin? ast)
        (cond
            [(equal? (bin-op ast) '+)
                (+ (eval (bin-fst ast)) (eval (bin-snd ast)))]
            [(equal? (bin-op ast) '*)
                (* (eval (bin-fst ast)) (eval (bin-snd ast)))]
            ; [else (apply-def 
            ;         (find-def (bin-op ast) defs) (bin-fst ast) (bin-snd ast))]
            )]
    [else ast]))

; find-def: Symbol (Listof def)
; Purpose: Return the def object matching name if found, else raise an error.

(define (find-def name defs)
    (cond
        [(empty? defs) (error "Name not found")]
        [(equal? name (def-name (first defs))) (first defs)]
        [else (find-def name (rest defs))]))

; (define (apply-def def arg1 arg2))