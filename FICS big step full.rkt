#lang htdp/isl+

; call: Symbol (Listof Symbol or Number)
(define-struct call (op args))

; f-let: (Listof def) (Listof AST)
(define-struct f-let (names expr))

; def: Symbol AST
(define-struct def (name val))

; lamb: Symbol (Listof Symbol) AST
(define-struct lamb (params expr))

; primitive?: Any -> boolean
; Purpose: Return whether an AST is a primitive value.

(define (primitive? expr) 
    (or (symbol? expr) (number? expr)))

; parse: sexp -> AST
; Purpose: Parse an s-expression to an AST.
; Examples

(define (parse sx)
    (cond
        [(list? sx)
            (cond
                [(equal? (first sx) 'let)
                    (parse-let (second sx) (rest (rest sx)))]
                ; skip function definition special form
                ; only (define name (lambda (param) (expr)))
                [(equal? (first sx) 'define)
                    (make-def (second sx) (parse (third sx)))]
                [(equal? (first sx) 'lambda)
                    (parse-lambda (second sx) (rest (rest sx)))]
                [else (make-call (first sx) (parse-all (rest sx)))])]
        [(primitive? sx) sx]
        [else (error "Parse error")]))

; parse-all (Listof sexp) -> (Listof AST)
; Purpose: Parse all s-expressions from a list.

(define (parse-all args)
    (cond
        [(empty? args) empty]
        [else (cons (parse (first args)) (parse-all (rest args)))]))

; parse-let: sexp -> AST
; Purpose: Helper function for parse. Returns the AST of a let expression.
;   Takes a list of name-value mappings as s-expressions as lft and an s-expression as rgt.

(define (parse-let fst snd)
    (make-f-let (parse-pairs fst) (parse-all snd)))

; parse-pairs (Listof sexp) -> (Listof defs)
; Purpose: Helper function for parse-let. 
;   Takes a list of name-value mappings as s-expressions. Builds a list of defs.

(define (parse-pairs sx-list)
    (cond
        [(empty? sx-list) empty]
        [else (let ([pair (first sx-list)])
            (cons 
                (make-def (first pair) (parse (second pair)))
                (parse-pairs (rest sx-list))))]))

(define (parse-lambda params expr)
    (make-lamb (parse-all params) (parse expr)))
    
 
;;;;;;;;;;; TODO vvvvvvvvvvvvvvvvvvvvvvvvv TODO evaluation, environments, lambda ;;;;;;;;;;;;;;;;;;;;

; closure: (Listof Symbol) AST (Listof def)
(define-struct closure (params expr env))

; eval: AST (Listof def) -> Number or Symbol
; Purpose: Evaluate a faux-racket AST.

(define (eval ast env)
    (cond
        ; [(f-let? ast) (do-let-form (f-let-names ast) (f-let-expr ast))]
        ; [(lamb? ast) (do-lambda-form (lamb-params ast) (lamb-expr ast))]
        [(call? ast) (f-apply (call-op ast) (call-args ast) env)]
        [(symbol? ast) (lookup ast env)]
        [(number? ast) ast]
        [else (error (format "Expected lambda, let, call, symbol, or number; found: ~a" ast))]))

; eval-all: (Listof AST) (Listof def) -> Number
; Purpose: Evaluate a list of ASTs and return the value of the last expression.

(define (eval-all ast-list last-val)
    (eval-all-helper ast-list last-val empty))

; eval-all-helper: (Listof AST) Number (Listof def) -> Number
; Purpose: Recursively evaluate a list of ASTs and return the value of the last expression.
(define (eval-all-helper ast-list last-val env)
    (cond
        [(empty? ast-list) last-val]
        [(def? (first ast-list))
            (let ([name (def-name (first ast-list))] [val (def-val (first ast-list))])
                (eval-all-helper (rest ast-list) last-val (add-to-env name val env)))]
        [else (eval-all-helper (rest ast-list) (eval (first ast-list) env) env)]))

; eval-args (Listof AST) (Listof def) -> (Listof Number)
; Purpose: Evaluate a list of ASTs and return a list of their values.

(define (eval-args ast-list env)
    (cond 
        [(empty? ast-list) empty]
        [else
            (cons (eval (first ast-list) env) (eval-args (rest ast-list) env))]))
    
; lookup: Symbol (Listof def) -> AST
; Purpose: Return the value associated with the symbol name in env.

(define (lookup name env)
    (cond
        [(empty? env) (error (format "Name ~a not found" name))]
        [(equal? name (def-name (first env))) (def-val (first env))]
        [else (lookup name (rest env))]))

; add-to-env: def (Listof def)
; Purpose: Add a new variable name and value to the environment. Create a closure
;   if the value associated with the name is a lambda.

(define (add-to-env name val env)
    (cond
        [(primitive? val) (cons (make-def name val) env)]
        [(lamb? val) (cons
            (make-def name 
                (make-closure (lamb-params val) (lamb-expr val) env)) env)]
        [else (cons (make-def name (eval val)) env)]
        ))

; f-apply: (Symbol or closure) (Listof Any) (Listof def) -> Number or Symbol
; Purpose: Apply the operator to the arguments passed in.

(define (f-apply op args env)
    (cond
        [(builtin? op) (apply-builtin op args env)]
        ; [(symbol? op) (f-apply (lookup op) args env)]
        ; [(closure? op) (dostuff)] ; TODO
        [else (error (format "f-apply: Expected an operator, found: ~a" op))]))

(define (builtin? name)
    (member name '(+ - * /)))

(define (apply-builtin op args env)
    (cond
        [(equal? '+ op) (apply + (eval-args args env))]
        [(equal? '- op) (apply - (eval-args args env))]
        [(equal? '* op) (apply * (eval-args args env))]
        [(equal? '/ op) (apply / (eval-args args env))]))
