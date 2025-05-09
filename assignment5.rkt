; FULLY IMPLEMENTED
#lang typed/racket

(require typed/rackunit)

;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Composite Type Defs ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Value (U NumV
                      BoolV
                      StrV
                      CloV
                      PrimV))

(define-type ExprC (U NumC
                      IdC
                      StrC
                      IfC
                      LamC
                      AppC))


  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Environment Defs ;;
  ;;;;;;;;;;;;;;;;;;;;;;

(struct Binding ([name : Symbol] [value : Value]) #:transparent)
(struct Env ([bindings : (Listof Binding)]) #:transparent)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Value Members Type Defs ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct NumV ([num : Real]) #:transparent)
(struct BoolV ([bool : Boolean]) #:transparent)
(struct StrV ([str : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Env]) #:transparent)
(struct PrimV ([fun : ((Listof Any) -> (U Value Nothing))]) #:transparent)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ExprC Members Type Defs ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct NumC ([num : Real]) #:transparent)
(struct IdC ([var : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([params : (Listof IdC)] [body : ExprC]) #:transparent)
(struct AppC ([expr : ExprC] [exprs : (Listof ExprC)]) #:transparent)


  ;;;;;;;;;;;;;;;;
  ;; Arity Defs ;;
  ;;;;;;;;;;;;;;;;

(define nullary 0)
(define unary 1)
(define binary 2)
(define ternary 3)



;;;;;;;;;;;;;;;;;;;;
;; Top Level Defs ;;
;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;
  ;; addPrim Def ;;
  ;;;;;;;;;;;;;;;;;

; This function takes in a list of args. If they are two numbers it returns
; their addition, else it throws an error.
(: addPrim ((Listof Any) -> NumV))
(define (addPrim args)
  (if (= (length args) binary)
     (match args
       [(list (NumV num1) (NumV num2)) (NumV (+ num1 num2))]
       [other (error 'addPrim "QTUM Either argument is not a number")])
     (error 'addPrim "QTUM Wrong arity. Expected ~e given ~e" binary (length args))))


  ;;;;;;;;;;;;;;;
  ;; minusPrim ;;
  ;;;;;;;;;;;;;;;

; This function takes in a list of args. If they are two numbers it returns
; their substraction, else it throws an error.
(: minusPrim ((Listof Any) -> NumV))
(define (minusPrim args)
  (if (= (length args) binary)
     (match args
       [(list (NumV num1) (NumV num2)) (NumV (- num1 num2))]
       [other (error 'minusPrim "QTUM Either argument is not a number")])
     (error 'minusPrim "QTUM Wrong arity. Expected ~e given ~e" binary (length args))))


  ;;;;;;;;;;;;;;
  ;; multPrim ;;
  ;;;;;;;;;;;;;;

; This function takes in a list of args. If they are two numbers it returns
; their multiplication, else it throws an error.
(: multPrim ((Listof Any) -> NumV))
(define (multPrim args)
  (if (= (length args) binary)
     (match args
       [(list (NumV num1) (NumV num2)) (NumV (* num1 num2))]
       [other (error 'multPrim "QTUM Either argument is not a number")])
     (error 'multPrim "QTUM Wrong arity. Expected ~e given ~e" binary (length args))))


  ;;;;;;;;;;;;;
  ;; divPrim ;;
  ;;;;;;;;;;;;;

; This function takes in a list of args. If they are two numbers and the second one
; is not 0 it returns their division, else it throws an error.
(: divPrim ((Listof Any) -> NumV))
(define (divPrim args)
  (if (= (length args) binary)
     (match args
       [(list (NumV num1) (NumV num2)) (if (= num2 0)
                                           (error 'divPrim "QTUM Division by 0")
                                           (NumV (/ num1 num2)))]
       [other (error 'divPrim "QTUM Either argument is not a number")])
     (error 'divPrim "QTUM Wrong arity. Expected ~e given ~e" binary (length args))))


  ;;;;;;;;;;;;;
  ;; ltePrim ;;
  ;;;;;;;;;;;;;

; This function takes in a list of args, and, if it contains two booleans,
; it applies the op <= on them. Otherwise, it throws an error.
(: ltePrim ((Listof Any) -> BoolV))
(define (ltePrim args)
  (if (= (length args) binary )
      (match args
        [(list (NumV num1) (NumV num2)) (BoolV (<= num1 num2))]
        [other (error 'ltePrim "QTUM Either argument is not a number")])
      (error 'ltePrim "QTUM Wrong arity. Expected ~e given ~e" binary (length args))))


  ;;;;;;;;;;;;;;;;
  ;; substrPrim ;;
  ;;;;;;;;;;;;;;;;

; This function takes in a string, and two natural numbers. It makes the substring
; starting at start and exclusively ending at stop. If stop < start throws and error.
(: substrPrim ((Listof Any) -> StrV))
(define (substrPrim args)
  (if (= (length args) ternary)
      (match args
        [(list (StrV str) (NumV start) (NumV end))
         (if (and (integer? start) 
                  (integer? end)
                  (>= start 0)
                  (<= end (string-length str))
                  (<= start end))
             (StrV (substring str (cast start Integer) (cast end Integer)))
             (error 'substrPrim 
                    "QTUM Invalid indices for substring: start=~a end=~a string-length=~a" 
                    start end (string-length str)))]
        [other (error 'substrPrim "QTUM Type mismatch, need a string and two integers")])
      (error 'substrPrim "QTUM Wrong arity. Expected 3 given ~e" (length args))))


  ;;;;;;;;;;;;;;;;
  ;; strlenPrim ;;
  ;;;;;;;;;;;;;;;;

; This function takes in a string and returns its length.
(: strlenPrim ((Listof Any) -> NumV))
(define (strlenPrim args)
  (if (= (length args) unary)
      (match args
        [(list (StrV str)) (NumV (string-length str))]
        [other (error 'strlenPrim "QTUM Argument is not a string")])
      (error 'strlenPrim "QTUM Wrong arity. Expected ~e given ~e" unary (length args))))


  ;;;;;;;;;;;;;;;
  ;; equalPrim ;;
  ;;;;;;;;;;;;;;;

; This function takes in a Value. If it is a NumV, BoolV, or StrV it compares them
; for equality and returns the result. Otherwise, it returns false.
(: equalPrim ((Listof Any) -> BoolV))
(define (equalPrim args)
  (if (= (length args) binary)
      (match args
        [(list (NumV n1) (NumV n2)) (BoolV (= n1 n2))]
        [(list (BoolV b1) (BoolV b2)) (BoolV (eq? b1 b2))]
        [(list (StrV s1) (StrV s2)) (BoolV (string=? s1 s2))]
        [other (BoolV #f)]) ; different types
      (error 'equalPrim "QTUM Wrong arity. Expected ~e given ~e" binary (length args))))


  ;;;;;;;;;;;;;;;
  ;; errorPrim ;;
  ;;;;;;;;;;;;;;;

; This function takes in a Value and raises an error containing the "user-error" substr
; and the Value.
(: errorPrim ((Listof Any) -> Nothing))
(define (errorPrim args)
  (cond [(= (length args) unary)
         (match args
           [(list (? Value? val)) (error 'errorPrim "QTUM user-error: ~a" (serialize (cast val Value)))]
           [_ (error 'errorPrim "QTUM Invalid value for error")])]
        [ else (error 'errorPrim "QTUM Wrong arity. Expected 1 given ~e" (length args))]))


  ;;;;;;;;;;;;;
  ;; println ;;
  ;;;;;;;;;;;;;

; This function takes in a string and prints it out in a newline. It
; always returns true.
(: println ((Listof Any) -> BoolV))
(define (println args)
  (cond [(= (length args) unary)
         (match args
           [(list (? StrV? val)) (BoolV (void? (displayln (StrV-str val))))]
           [_ (error 'println "QTUM Invalid value type: needs a string")])]
        [ else (error 'println "QTUM Wrong arity. Expected 1 given ~e" (length args))]))


  ;;;;;;;;;;;;;;
  ;; read-num ;;
  ;;;;;;;;;;;;;;

; This function does not get any inputs. It returns the real number
; read from stdin.
(: read-num ((Listof Any) -> NumV))
(define (read-num args)
  (cond [(= (length args) nullary)
         (printf "> ")
         (define inputln (read-line))
         (define result (string->number (if (string? inputln)
                                            inputln
                                            "")))
         (match result
           [#f (error 'read-num "QTUM input is not a number")]
           [(? real? n) (NumV n)]
           [other (error 'read-num "QTUM input is not a real number")])]
        [ else (error 'read-num "QTUM Wrong arity. Expected ~e given ~e" nullary (length args))]))


  ;;;;;;;;;;;;;;
  ;; read-str ;;
  ;;;;;;;;;;;;;;

; This function does not take any inputs. It reads a string from
; stdin.
(: read-str ((Listof Any) -> StrV))
(define (read-str args)
  (cond [(= (length args) nullary)
         (printf "> ")
         (define inputln (read-line))
         (if (string? inputln)
                            (StrV (string-trim inputln))
                            (StrV ""))]
         [else (error 'read-str "QTUM Wrong arity. Expected ~e given ~e" nullary (length args))]))


  ;;;;;;;;;
  ;; seq ;;
  ;;;;;;;;;

; This function takes in a list of values and it returns the last one.
(: seq ((Listof Any) -> Value))
(define (seq args)
  (match args
    ['() (error 'read-str "QTUM Wrong arity. Expected one or more given ~e" (length args))]
    [(list vals ...) (cast (last vals) Value)]))


  ;;;;;;;;;;;;
  ;; strCat ;;
  ;;;;;;;;;;;;

; This function takes in a list of values, and stringifies them.
; Then it returns a string, the result of concatenating all of them.
(: strCat ((Listof Any) -> Value))
(define (strCat args)
  (match args
    ['() (error 'read-str "QTUM Wrong arity. Expected one or more given ~e" (length args))]
    [(list vals ...) (define string-list (map (lambda ([arg : Value]) (if (StrV? arg)
                                                                          (StrV-str arg)
                                                                          (serialize arg)))
                                              (cast vals (Listof Value))))
                     (StrV (apply string-append string-list))]))


  ;;;;;;;;;;;;;;;;;
  ;; Top Env Def ;;
  ;;;;;;;;;;;;;;;;;

(define top-env (Env (list
                      (Binding 'true (BoolV #t))
                      (Binding 'false (BoolV #f))
                      (Binding '+ (PrimV addPrim))
                      (Binding '- (PrimV minusPrim))
                      (Binding '* (PrimV multPrim))
                      (Binding '/ (PrimV divPrim))
                      (Binding '<= (PrimV ltePrim))
                      (Binding 'substring (PrimV substrPrim))
                      (Binding 'strlen (PrimV strlenPrim))
                      (Binding 'equal? (PrimV equalPrim))
                      (Binding 'error (PrimV errorPrim))
                      (Binding 'println (PrimV println))
                      (Binding 'read-num (PrimV read-num)) 
                      (Binding 'read-str (PrimV read-str))
                      (Binding 'seq (PrimV seq))
                      (Binding '++ (PrimV strCat))
                      )))



;;;;;;;;;;;;;;;;;;;;
;; Main Functions ;;
;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;
  ;; top-interp ;;
  ;;;;;;;;;;;;;;;;

; This function takes in an expression (program) and both parses it and interprets it.
; It returns a serialized value.
(: top-interp (Sexp -> String))
(define (top-interp s)
  (serialize (interp (parse s) top-env)))


  ;;;;;;;;;;;;
  ;; Interp ;;
  ;;;;;;;;;;;;

; This function takes in an ExprC and an Env, and it interprets the expr
; into a value.
(: interp (ExprC Env -> Value))
(define (interp expr currEnv)
  (match expr
    [(NumC n) (NumV n)]
    [(IdC var) (lookup var currEnv)]
    [(StrC str) (StrV str)]
    [(IfC test then else) (define test-res (interp test currEnv))
                          (match test-res
                            [(BoolV bool) (if bool
                                              (interp then currEnv)
                                              (interp else currEnv))]
                            [other (error 'interp "QTUM test ~e is not a Bool" expr)])]
    [(LamC params body) (CloV (map IdC-var params) body currEnv)]
    [(AppC proc args) (match (interp proc currEnv)
                        [(CloV params body clo-env)
                         (cond
                           [(= (length args) (length params))
                            (define argval (map (lambda ([arg : ExprC]) (interp arg currEnv)) args))
                            (define new-env (extendEnv clo-env params argval))
                            (interp body new-env)]
                           [else (error 'interp "QTUM Incorrect number of arguments in ~e given ~e expected ~e"
                                        expr (length args) (length params))])]
                        [(PrimV fun)
                         (define argval (map (lambda ([arg : ExprC]) (interp arg currEnv)) args))
                         (fun argval)]
                        [other (error 'interp "QTUM Attempting to do application on non-function data type")])]))


  ;;;;;;;;;;;
  ;; Parse ;;
  ;;;;;;;;;;;

; This function takes in an Sexp and produces a valid QTUM4 ExprC.
(: parse (Sexp -> ExprC))
(define (parse s)
  (match s
    [(? real? r) (NumC r)]
    [(? string? str) (StrC str)]
    [(list 'if test then else) (IfC (parse test) (parse then) (parse else))]
    [(list 'with (list (? symbol? ids) '= exprs) ... exprC)
     (define params (cast (map parse (cast ids (Listof Sexp))) (Listof IdC)))
     (if (areParamsValid? params)
         (AppC (LamC params (parse exprC))
               (cast (map parse (cast exprs (Listof Sexp))) (Listof ExprC)))
         (error 'parse "QTUM Invalid parameters: duplicated elements"))]
    [(list (? symbol? ids) ... '=> exprC) (define params (cast (map parse (cast ids (Listof Sexp))) (Listof IdC)))
                                          (if (areParamsValid? params)
                                              (LamC params (parse exprC))
                                              (error 'parse "QTUM Invalid parameters: duplicated elements ~e" s))]
    [(? symbol? s) (if (legalsymbol? s)
                       (IdC s)
                       (error 'parse "QTUM Illegal identifier use: ~e" s))]
    [(list proc args ...) (AppC (parse proc) (map parse args))]
    [other (error 'parse "QTUM Invalid syntax: ~s" s)]))


  ;;;;;;;;;;;;;;;
  ;; Serialize ;;
  ;;;;;;;;;;;;;;;

; This function takes in a Value and transforms it into a string (returns a string).
(: serialize (Value -> String))
(define (serialize value)
  (match value
    [(NumV num) (~v num)]
    [(BoolV b) (if b "true" "false")]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _) "#<primop>"]))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Function Defs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;
  ;; Value? ;;
  ;;;;;;;;;;;;

; This function takes in anything and returns true if it is a Value.
; Otherwise, it returns false.
(: Value? (Any -> Boolean))
(define (Value? test)
  (match test
    [(? NumV?) #t]
    [(? BoolV?) #t]
    [(? StrV?) #t]
    [(? CloV?) #t]
    [(? PrimV?) #t]
    [other #f]))


  ;;;;;;;;;;;;
  ;; lookup ;;
  ;;;;;;;;;;;;

; This function takes in a symbol for lookup and an env to look into.
; returns the value of the binding associated with the symbol or an error (if not found).
(: lookup (Symbol Env -> Value))
(define (lookup for env)
  (match (Env-bindings env)
    ['() (error 'lookup "QTUM Reference not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for (Env r))])]))


  ;;;;;;;;;;;;;;;
  ;; contains? ;;
  ;;;;;;;;;;;;;;;

; This function takes in an element and a list, and checks if such element
; is contained in the list. Return true if found, false otherwise.
(: contains? (Any (Listof Any) -> Boolean))
(define (contains? element list)
  (if (not (member element list))
      #f
      #t))


  ;;;;;;;;;;;;;;;;;;
  ;; legalsymbol? ;;
  ;;;;;;;;;;;;;;;;;;

; This function takes in a symbol and determines if it is a valid
; variable reference in QTUM4. Returns true if it is, false otherwise.
(: legalsymbol? (Symbol -> Boolean))
(define (legalsymbol? symbol)
  (not (contains? symbol '(if => with =))))


  ;;;;;;;;;;;;;;;;;;
  ;; makeBindings ;;
  ;;;;;;;;;;;;;;;;;;

; This function takes in a list of IdC's and Values and makes a list of bindings.
; It is a helper to extendEnv
(: makeBindings ((Listof Symbol) (Listof Value) -> (Listof Binding)))
(define (makeBindings params args)
  (match* (params args)
    [((cons f1 r1) (cons f2 r2)) (cons (Binding f1 f2) (makeBindings r1 r2))]
    [('() '()) '()]))


  ;;;;;;;;;;;;;;;
  ;; extendEnv ;;
  ;;;;;;;;;;;;;;;

; This function takes in an environment, a list of IdC's, and a list of values. It makes
; bindings out of the IdC's and values and appends them to the front of the environment
(: extendEnv (Env (Listof Symbol) (Listof Value) -> Env))
(define (extendEnv currEnv params args)
  (match currEnv
    [(Env bindings) (Env (append (makeBindings params args) (Env-bindings currEnv)))]))


  ;;;;;;;;;;;;;;;;;;;;;
  ;; areParamsValid? ;;
  ;;;;;;;;;;;;;;;;;;;;;

; This function takes a list of symbols and checks if there are any
; duplicates (returns false if there's duplicates, true otherwise).
; It is a helper function to parse.
(: areParamsValid? ((Listof IdC) -> Boolean))
(define (areParamsValid? symList)
  (match symList
    ['() #t]
    [(cons f r) (if (not (member f r))
                    (areParamsValid? r)
                    #f)]))



;;;;;;;;;;;
;; Tests ;;      ;; Module style testing
;;;;;;;;;;;

; addPrim test
(check-equal? (addPrim (list (NumV 3) (NumV 2))) (NumV 5))
(check-exn (regexp "addPrim: QTUM Either argument is not a number")
           (lambda () (addPrim (list (NumV 3) (IdC 'x)))))
(check-exn (regexp "addPrim: QTUM Wrong arity. Expected 2 given 3")
           (lambda () (addPrim (list (NumV 3) (NumV 3) (NumV 3)))))

; minusPrim test
(check-equal? (minusPrim (list (NumV 3) (NumV 2))) (NumV 1))
(check-exn (regexp "minusPrim: QTUM Either argument is not a number")
           (lambda () (minusPrim (list (NumV 3) (IdC 'x)))))
(check-exn (regexp "minusPrim: QTUM Wrong arity. Expected 2 given 3")
           (lambda () (minusPrim (list (NumV 3) (NumV 3) (NumV 3)))))

; multPrim test
(check-equal? (multPrim (list (NumV 3) (NumV 2))) (NumV 6))
(check-exn (regexp "multPrim: QTUM Either argument is not a number")
           (lambda () (multPrim (list (NumV 3) (IdC 'x)))))
(check-exn (regexp "multPrim: QTUM Wrong arity. Expected 2 given 3")
           (lambda () (multPrim (list (NumV 3) (NumV 3) (NumV 3)))))

; divPrim test
(check-equal? (divPrim (list (NumV 3) (NumV 3))) (NumV 1))
(check-exn (regexp "divPrim: QTUM Either argument is not a number")
           (lambda () (divPrim (list (NumV 3) (IdC 'x)))))
(check-exn (regexp "divPrim: QTUM Wrong arity. Expected 2 given 3")
           (lambda () (divPrim (list (NumV 3) (NumV 3) (NumV 3)))))
(check-exn (regexp "divPrim: QTUM Division by 0")
           (lambda () (divPrim (list (NumV 3) (NumV 0)))))

; ltePrim test
(check-equal? (ltePrim (list (NumV 3) (NumV 3))) (BoolV #t))
(check-equal? (ltePrim (list (NumV 4) (NumV 3))) (BoolV #f))
(check-exn (regexp "ltePrim: QTUM Either argument is not a number")
           (lambda () (ltePrim (list (NumV 3) (IdC 'x)))))
(check-exn (regexp "ltePrim: QTUM Wrong arity. Expected 2 given 3")
           (lambda () (ltePrim (list (NumV 3) (NumV 3) (NumV 3)))))

; substr test
(check-equal? (substrPrim (list (StrV "hello") (NumV 1) (NumV 3))) (StrV "el"))
(check-equal? (substrPrim (list (StrV "racket") (NumV 0) (NumV 6))) (StrV "racket"))
(check-equal? (substrPrim (list (StrV "world") (NumV 1) (NumV 4))) (StrV "orl"))
(check-exn (regexp "QTUM Wrong arity. Expected 3 given 2")
          (lambda () (substrPrim (list (StrV "hi") (NumV 1))))) ; Too few arguments
(check-exn (regexp "QTUM Wrong arity. Expected 3 given 4")
          (lambda () (substrPrim (list (StrV "hi") (NumV 1) (NumV 2) (NumV 3))))) ; Too many arguments
(check-exn (regexp "QTUM Invalid indices")
          (lambda () (substrPrim (list (StrV "hi") (NumV 1.5) (NumV 2))))) ; Float start
(check-exn (regexp "QTUM Invalid indices.*start=-1")
          (lambda () (substrPrim (list (StrV "hi") (NumV -1) (NumV 2)))))
(check-exn 
  (regexp "QTUM Type mismatch, need a string and two integers")
  (lambda () 
    (substrPrim (list (BoolV #t) (NumV 1) (NumV 2)))) "Boolean instead of string")

; strlen test
(check-equal? (strlenPrim (list (StrV ""))) (NumV 0) "Empty string")
(check-equal? (strlenPrim (list (StrV "a"))) (NumV 1) "Single char")
(check-equal? (strlenPrim (list (StrV "hello"))) (NumV 5) "Multi-char string")
(check-exn 
  (regexp "QTUM Argument is not a string")
  (lambda () (strlenPrim (list (NumV 42)))))
(check-exn 
  (regexp "QTUM Argument is not a string")
  (lambda () (strlenPrim (list (BoolV #t)))))
(check-exn 
  (regexp "QTUM Wrong arity. Expected 1 given 0")
  (lambda () (strlenPrim '())) 
  "No arguments")
(check-exn 
  (regexp "QTUM Wrong arity. Expected 1 given 2")
  (lambda () (strlenPrim (list (StrV "hi") (StrV "there")))) 
  "Too many arguments")

; equal? test
(check-equal? (equalPrim (list (NumV 3) (NumV 3))) (BoolV #t) "Equal numbers")
(check-equal? (equalPrim (list (NumV 3) (NumV 4))) (BoolV #f) "Unequal numbers")
(check-equal? (equalPrim (list (BoolV #t) (BoolV #t))) (BoolV #t) "Equal booleans (true)")
(check-equal? (equalPrim (list (StrV "hi") (StrV "hi"))) (BoolV #t) "Equal strings")
(check-equal? (equalPrim (list (StrV "hi") (StrV "bye"))) (BoolV #f) "Unequal strings")
(check-equal? (equalPrim (list (NumV 3) (StrV "3"))) (BoolV #f) "Number vs string")
(check-equal? (equalPrim (list (BoolV #t) (NumV 1))) (BoolV #f) "Boolean vs number")
(check-equal? (equalPrim (list (StrV "true") (BoolV #t))) (BoolV #f) "String vs boolean")
(check-exn 
  (regexp "equalPrim: QTUM Wrong arity. Expected 2 given 1")
  (lambda () (equalPrim (list (NumV 3)))) 
  "Too few arguments")
(check-equal? (equalPrim (list (BoolV #t) (BoolV #t))) (BoolV #t) "true == true")
(check-equal? (equalPrim (list (BoolV #f) (BoolV #f))) (BoolV #t) "false == false")
(check-equal? (equalPrim (list (BoolV #t) (BoolV #f))) (BoolV #f) "true != false")

; errorPrim test
(check-exn (regexp "QTUM user-error: 42")
          (lambda () (errorPrim (list (NumV 42)))))
(check-exn (regexp "QTUM user-error: true")
          (lambda () (errorPrim (list (BoolV #t)))))
(check-exn (regexp "QTUM user-error: \"hello\"")
          (lambda () (errorPrim (list (StrV "hello")))))
(check-exn (regexp "QTUM user-error: #<procedure>")
          (lambda () (errorPrim (list (CloV '() (NumC 0) (Env '()))))))
(check-exn (regexp "QTUM user-error: #<primop>")
          (lambda () (errorPrim (list (PrimV addPrim)))))
(check-exn (regexp "QTUM Invalid value for error")
          (lambda () (errorPrim (list "not a QTUM value"))))
(check-exn (regexp "QTUM Wrong arity. Expected 1 given 0")
          (lambda () (errorPrim '())))
(check-exn (regexp "QTUM") 
          (lambda () (errorPrim (list (NumV 1)))))
(check-exn (regexp "QTUM")
          (lambda () (errorPrim '())))

;; lookup test
(check-equal? (lookup '+ top-env) (PrimV addPrim))
(check-exn (regexp "Reference not found: 'askdfhjslf")
           (lambda () (lookup 'askdfhjslf top-env)))

; Serialize test
(check-equal? (serialize (NumV 3)) "3")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "Hi")) "\"Hi\"")
(check-equal? (serialize (CloV '(x) (IdC 'x) (Env '()))) "#<procedure>")
(check-equal? (serialize (PrimV addPrim)) "#<primop>")

; contains? tests
(check-equal? (contains? 'x '(x y)) #t)
(check-equal? (contains? 'x '(y)) #f)

; legalsymbol? tests
(check-equal? (legalsymbol? '+) #t)
(check-equal? (legalsymbol? '=) #f)

; parse tests
(check-equal? (parse '3) (NumC 3))
(check-equal? (parse '(if 1 2 3)) (IfC (NumC 1) (NumC 2) (NumC 3)))
(check-equal? (parse '"Hi") (StrC "Hi"))
(check-equal? (parse '{with [x = 3] x})
              (AppC (LamC (list (IdC 'x)) (IdC 'x)) (list (NumC 3))))
(check-equal? (parse '{with [x = 3] [y = 4] x})
              (AppC (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x)) (list (NumC 3) (NumC 4))))
(check-equal? (parse '(x y => x)) (LamC (list (IdC 'x) (IdC 'y)) (IdC 'x)))
(check-equal? (parse '{{x => x} 3}) (AppC (LamC (list (IdC 'x)) (IdC 'x)) (list (NumC 3))))
(check-exn (regexp "parse: QTUM Illegal identifier use: 'with")
           (lambda () (parse '(with))))
(check-exn (regexp "parse: QTUM Invalid syntax: ")
           (lambda () (parse '{})))

; top-interp tests
(check-equal? (top-interp '{{x => x} 3}) "3")
(check-equal? (top-interp '{{str => {strlen str}} "Hiii"}) "4")
(check-equal? (top-interp '{if true 3 2}) "3")
(check-equal? (top-interp '{if false 3 2}) "2")
;(check-equal? (top-interp '{seq
;                            {println "What is your favorite integer between 6 and 7?"}}) "true")
;(check-equal? (top-interp '{seq
; {println "What is your favorite integer between 6 and 7?"}
; {with [your-number = {read-num}]
;    {println {++ "Interesting, you picked " your-number ". Bold choice!"}}}}) "true")
(check-exn (regexp "Invalid parameters.*duplicated elements")
           (lambda () (top-interp '{{x x => x} 3})))
(check-exn (regexp "is not a Bool")
           (lambda () (top-interp '{if + 1 3})))
(check-exn (regexp "Incorrect number of arguments.*given 1 expected 0")
           (lambda () (top-interp '{{=> 3} 3})))
(check-exn (regexp "Invalid parameters.*duplicated elements")
           (lambda () (top-interp '{with [x = 3] [x = 3] x})))
(check-exn (regexp "Attempting to do application on non-function data type")
           (lambda () (top-interp '(3 4 5))))

; extendEnv tests
(check-equal? (extendEnv top-env (list 'x) (list (NumV 3)))
              (Env (cons (Binding 'x (NumV 3)) (Env-bindings top-env))))

; println test
(check-exn (regexp "needs a string")
           (lambda () (println (list (BoolV #f)))))
(check-exn (regexp "Wrong arity.*Expected 1 given 2")
           (lambda () (println (list (BoolV #f) (BoolV #f)))))

; read-num tests
(check-exn (regexp "Wrong arity.*Expected 0 given 2")
           (lambda () (read-num (list (BoolV #f) (BoolV #f)))))

; read-str test
(check-exn (regexp "Wrong arity.*Expected 0 given 2")
           (lambda () (read-str (list (BoolV #f) (BoolV #f)))))

; seq test
(check-equal? (seq (list (BoolV #t) (NumV 3))) (NumV 3))
(check-equal? (seq (list (BoolV #t))) (BoolV #t))
(check-exn (regexp "Wrong arity.*Expected one or more given 0")
           (lambda () (seq '())))


;;;;;;;;;;
;; GAME ;;
;;;;;;;;;;

(top-interp '{seq
              {println "This program computes an approximation of sin using Taylor series.
Please input the x term (real number) and the number of terms to compute (integer)"}
              {with
               [pow = {base exp r => {if {<= exp 0}
                                         1
                                         {* base {r base {- exp 1} r}}}}]
               [fact = {x r => {if {equal? x 1}
                                   x
                                   {* x {r {- x 1} r}}}}]
               {with [sin = {x n r => {if {<= n 0}
                                          1
                                          {+ {* {pow -1 n pow} {/ {pow x {+ {* 2 n} 1} pow} {fact {+ {* 2 n} 1} fact}}}
                                             {r x {- n 1} r}}}}]
                     [x-val = {read-num}]
                     [n-val = {read-num}]
                     
                     {sin x-val n-val sin}}}})
