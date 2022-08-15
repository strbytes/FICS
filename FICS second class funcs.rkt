#lang htdp/isl+
; #lang racket


; FOR A PROPER INTERPRETER
; validate: check for appropriate argument structure
;   probably need variations for different forms
; parse and eval should be able to process lists of operations
; parse and eval should be able to process in-line define statments
;   parse: just put the def struct in the ast
;   eval: if a def is found, call eval on rest of statements with new def
;       consed to front of def list
; regular definitions
;   definitions just point to a val, no def special form
;       define function special form just gives a name to a lambda


; call: Symbol (Listof Symbol or Number)
(define-struct call (op args))

; parse: sexp -> AST
; Purpose: Parse an s-expression to an AST. Supports let and call expressions.
; Examples
; (check-expect (parse '(let ([a 1] [b 2]) (+ a b c)))
;     (make-f-let 
;         (list (make-pair 'a 1) (make-pair 'b 2)) (make-call '+ '(a b c))))

(define (parse sx)
  (cond
    [(and (list? sx) (equal? (first sx) 'let))
        (parse-let (second sx) (third sx))]
    [(and (list? sx) (symbol? (first sx))) (make-call (first sx) (parse-args (rest sx)))]
    [(or (symbol? sx) (number? sx)) sx]
    [else (error "Parse error")]))

; parse-args: (Listof sexp) -> (Listof AST)
; Purpose: Parse all args from a call expression.

(define (parse-args args)
    (cond
        [(empty? args) empty]
        [else (cons (parse (first args)) (parse-args (rest args)))]))


; def: Symbol (Listof Symbol) AST
(define-struct def (name params expr))

; parse-def-list: (Listof sexp) -> (Listof def)
; Purpose: Parse a list of define statements and return a list of def structs.
(define (parse-def-list sx-list)
    (cond
        [(empty? sx-list) empty]
        [else (cons (parse-def (first sx-list)) (parse-def-list (rest sx-list)))]))

; parse-def: sexp -> def
; Purpose: Parse a single define statement.
(define (parse-def sx)
    (cond
        [(and (list? sx) (equal? (first sx) 'define))
            (let (
                [name (first (first (rest sx)))]
                [args (rest (first (rest sx)))]
                [expr (parse (first (rest (rest sx))))])
                (make-def name args expr))]))

(define-struct f-let (names expr))
(define-struct pair (name val))

; parse-let: sexp -> AST
; Purpose: Helper function for parse. Returns the AST of a let expression.
;   Takes a list of name-value mappings as s-expressions as lft and an s-expression as rgt.
; Examples:
; (check-expect (parse-let '([a 1] [b 2]) '(+ a b))
;     (make-f-let (list (make-pair 'a 1) (make-pair 'b 2)) (make-call '+ '(a b))))

(define (parse-let fst snd)
    (make-f-let (parse-pairs fst) (parse snd)))

; parse-pairs: sexp -> (Listof pairs)
; Purpose: Helper function for parse-let. 
;   Takes a list of name-value mappings as s-expressions. Builds a list of pairs.
; Examples:
; (check-expect (parse-pairs '([a 1] [b 2])) (list (make-pair 'a 1) (make-pair 'b 2)))

(define (parse-pairs pr)
    (cond
        [(empty? pr) empty]
        [else (cons (make-pair (first (first pr)) (parse (second (first pr)) )) 
            (parse-pairs (rest pr)))]))

; eval: AST -> number
; Purpose: Evaluate a faux-racket AST
; Example:
; (check-expect (eval (parse '(let ([a 1] [b 2]) (+ a b)))) 3)
; (check-expect (eval (parse '(a 1 2)) (parse-def-list '((define (a b c) (+ b c))))) 3)
 
(define (eval ast defs)
  (cond
    [(f-let? ast) (eval (do-let-form (f-let-names ast) (f-let-expr ast)) defs)]
    [(call? ast)
        (cond
            [(member (call-op ast) builtins)
                (eval-builtin ast defs)]
            [else (let ([def (find-def (call-op ast) defs)])
                (eval (apply-def 
                    (def-params def)
                    (call-args ast)
                    (def-expr def) defs) defs))])]
    [else ast]))

(define builtins '(+ - * /))

(define (eval-builtin ast defs)
    (cond
        [(equal? '+ (call-op ast)) (apply + (eval-args (call-args ast) defs))]
        [(equal? '- (call-op ast)) (apply - (eval-args (call-args ast) defs))]
        [(equal? '* (call-op ast)) (apply * (eval-args (call-args ast) defs))]
        [(equal? '/ (call-op ast)) (apply / (eval-args (call-args ast) defs))]))

(define (eval-args args defs)
    (cond
        [(empty? args) empty]
        [else (cons (eval (first args) defs) (eval-args (rest args) defs))]))

(define (find-def name defs)
    (cond
        [(empty? defs) (error "Name not found")]
        [(equal? name (def-name (first defs))) (first defs)]
        [else (find-def name (rest defs))]))

(define (apply-def params args expr defs)
    (cond
        [(and (empty? params) (empty? args)) expr]
        [(or (empty? params) (empty? args)) (error "Invalid number of arguments")]
        [else (apply-def 
                (rest params) 
                (rest args) 
                (sub (first params) (first args) expr defs) defs)]))

(define (sub param val expr defs)
    (cond
        [(f-let? expr)
            (let ([this (f-let-expr expr)]) (make-f-let 
                (sub-pairs param val (f-let-names expr) defs)
                (make-call 
                    (if (equal? param (call-op this)) val (call-op this))
                    (sub-args param val (call-args this) defs))))]
        [(call? expr) 
            (make-call 
                (if (equal? param (call-op expr)) val (call-op expr))
                (sub-args param val (call-args expr) defs))]
        [(equal? param expr) val]
        [else expr]))

(define (sub-args param val arg-list defs)
    (cond
        [(empty? arg-list) empty]
        [else (cons 
            (sub param val (first arg-list) defs)
            (sub-args param val (rest arg-list) defs))]))

(define (sub-pairs param val names defs)
    (cond
        [(empty? names) empty]
        [else (cons 
            (sub-pair param val (first names) defs)
            (sub-pairs param val (rest names) defs))]))

(define (sub-pair param val name defs)
    (make-pair 
        (if (equal? (pair-name name) param) val (pair-name name))
        (sub param val (pair-val name) defs)))

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

(define (do-let-once pr expr)
    (cond
        [(f-let? expr)
            (do-let-once pr (do-let-form (f-let-names expr) (f-let-expr expr)))]
        [(call? expr)
            (make-call 
                (if (equal? (call-op expr) (pair-name pr)) (pair-val pr) (call-op expr))
                (sub-let pr (call-args expr)))]
        [(equal? (pair-name pr) expr) (pair-val pr)]
        [else expr]))

(define (sub-let pr arg-list)
    (cond
        [(empty? arg-list) empty]
        [else (cons 
            (do-let-once pr (first arg-list))
            (sub-let pr (rest arg-list)))]))


(define test-defs (parse-def-list '(
    (define (lb-kg lb) (/ lb 2.2))
    (define (kg-lb kg) (* kg 2.2))
    (define (test-let a) (let ([b a]) (* b b)))
    (define (op a b c) (a b c))
    (define (let-op a b c) (let ([d a] [e b] [f c]) (d e f)))
    )))


(define test-lb (parse '(lb-kg 225)))
(define test-kg (parse '(kg-lb 100)))
(define test-both (parse '(lb-kg (kg-lb 100))))
(define test-let (parse '(test-let 3)))
(define test-op (parse '(op * 2 3)))
(define test-let-op (parse '(let-op * 2 3)))


; SO FAR
; let and def seem to work together
; can sub both operations and arguments
; feels way too complex:
    ; i liked the 61a interpreter's approach of just parsing lists
    ; and checking special forms inside the evaluator
    ; don't like having seperate "small step" and "big step" evaluators
        ; not really even sure what the difference is exactly
; oh come to think of it the parser wouldn't do anything for a list-based ast lol
; TODO
; small step evaluation of local def ?
    ; honestly may just skip ahead to lambdas and environments with big step
    ; then go for small step once i have a more generalized model of functions
    ; and substitutions