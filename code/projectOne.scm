; GROUP 15 INTERPRETER PART ONE
; Caleb Cain, Grace Dai, Savita Medlang

; Parses file, loads each line into interpreter
; determines the correct return value


; INTERPRETER PT 4:

(load "classParser.scm")
(load "state.scm")

; the initial state
(define M-state-init '((()())))

;evaluates each line
(define M-state
  (lambda (parse-tree state gotos class instance)
       (cond
         ((null? parse-tree) state)
         ((declaration-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-declare (branch parse-tree) state gotos class instance) gotos class instance)) ;var
         ((while-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-while (branch parse-tree) state gotos class instance) gotos class instance));while
         ((if-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-if (branch parse-tree) state gotos class instance) gotos class instance));if ;return
         ((assign-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-assign (branch parse-tree) state gotos class instance) gotos class instance)) ;=
         ((break-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-break state gotos class instance) gotos class instance))
         ((try-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-try (branch parse-tree) (addLayer state) gotos class instance) gotos class instance))
         ((throw-stmt? (branch parse-tree)) (M-state-throw (branch parse-tree) state gotos class instance))
         ((continue-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-continue (branch parse-tree) state gotos class instance) gotos class instance))
         ((return-stmt? (branch parse-tree)) (M-state-return (branch parse-tree) state gotos class instance))
         ((begin-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-begin (cdr (branch parse-tree)) (addLayer state) gotos class instance) gotos class instance))
         ((func-def-stmt? (branch parse-tree))  (M-state (cdr parse-tree) (M-state-func-def (branch parse-tree) state gotos class instance) gotos class instance))
         ((func-call-stmt? (branch parse-tree)) (M-state (cdr parse-tree) (M-state-func-call (branch parse-tree) state gotos class instance) gotos class instance))
         (else 'badoperator "illegal expression"))))

(define functionname cadr)
(define functionparams caddr)
(define functionbody cadddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; PART 4 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the constants for class name, class body, and parent class name of each class
(define class-name cadr)
(define parent-class-name
  (lambda (class-def)
    (if (null? (caddr class-def))
        'null
        (cadr (caddr class-def)))))
(define class-body cadddr)

; M-object for evaluating 
(define M-object
 (lambda (oexpression state gotos class instance)
   (cond
     ((eq? 'this oexpression) (list (get-instance-type instance)instance))
     ((eq? 'super oexpression) (list (get-parent-class (get-class class (get-class-layer state))) instance))
     ((and (pair? oexpression) (new-stmt? oexpression)) (list (cadr oexpression) (new-instance (cadr oexpression) state)))
     ((and (pair? oexpression) (funcall? oexpression)) (list class (M-value oexpression state gotos class instance)))
     ; a variable
     (else ((lambda (ninstance)
              (list (get-instance-type ninstance) ninstance))
            (if (contains-var? oexpression state)
                      (M-state-getVar state oexpression)
                      (unbox (get-instance-var oexpression instance class state))))))))

;; function to initialize each of the classes
(define initialize-classes
  (lambda (parse-tree state gotos class instance)
    (cond
      ((null? parse-tree) state)
      (else (initialize-classes (cdr parse-tree)
                                (M-state-setVar (M-state-declare-var (list (class-name (branch parse-tree))) state gotos-init)
                                                 (class-name (branch parse-tree))
                                                 (new-class (parent-class-name (branch parse-tree))
                                                             (get-class (parent-class-name (branch parse-tree))
                                                                           (get-class-layer state))
                                                             (car (initialize-class-body (class-body (branch parse-tree))
                                                                                          M-state-init M-state-init gotos-init
                                                                                          (class-name (branch parse-tree)) 'no-instance-a))
                                                             (cadr (initialize-class-body (class-body (branch parse-tree))
                                                                                           M-state-init M-state-init gotos-init
                                                                                           (class-name (branch parse-tree)) 'no-instance-b)))
                                                 gotos class instance) gotos class instance)))))
                                          
;; initializes a class
;; correctly initializes the variables (properties) and methods (functions) of each class
;; also keeps track of the class' parent class
(define initialize-class-body
  (lambda (parse-tree property-state method-state gotos class instance)
    (cond
      ((null? parse-tree) (list property-state method-state))
      ((func-def-stmt? (branch parse-tree)) (initialize-class-body
                                             (cdr parse-tree) property-state
                                                                   (M-state-func-def (branch parse-tree)
                                                                                    method-state gotos class instance)
                                                                   gotos class instance))
      ((declaration-stmt? (branch parse-tree)) (initialize-class-body
                                                    (cdr parse-tree) (M-state-declare
                                                                      (branch parse-tree)
                                                                      property-state gotos class instance)
                                                    method-state gotos class instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; PART 3 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; define a function
(define M-state-func-def
  (lambda (lis state gotos class instance)
    (M-state-setVar (M-state-declare-var (list (functionname lis)) state gotos) (functionname lis) (create-closure (functionname lis) (functionparams lis) (functionbody lis) gotos class) gotos class instance)))

; calls a function with the state class and instance
(define M-state-func-call
  (lambda (func-call state gotos class instance)
    (begin (M-value func-call state gotos class instance) state)))

; create a closure for a function
(define create-closure
  (lambda (func-name params body gotos class)
    (lambda (actual-params funcall-env instance)
      (M-get-return
       (call/cc
        (lambda (return)
          (begin
            (M-state body (M-state-create-env params actual-params funcall-env)
                     (gotos-set-return return gotos) class instance))))))))

; create environment to evaluate a function in
(define M-state-create-env
  (lambda (params actual-params state)
    (addWorkingLayer (construct-layer params actual-params) state)))

; adds a working layer on to the state as an environment
(define addWorkingLayer
  (lambda (layer state)
    (cons layer state)))

; environment for a function call
(define M-state-funcall-env
  (lambda (func-name state)
    (cond
      ((null? state) (error "Function not found"))
      ((layer-contains-var? func-name (car (car state))) state)
      (else (M-state-funcall-env func-name (removeLayer state))))))

; evaluates if condition, determines if there is an else condition or not
(define M-state-if
  (lambda (lis state gotos class instance)
    (if (null? (cdr (cdr (cdr lis))))
        (M-state-if-no-else (firstparam lis) (secondparam lis) state gotos class instance)
        (M-state-if-else (firstparam lis) (secondparam lis) (thirdparam lis) state gotos class instance))))

; if condition, no else
(define M-state-if-no-else
  (lambda (cond then state gotos class instance)
    (if (M-bool cond state gotos class instance)
        (M-state (list then) state gotos class instance)
        state)))

; if statement with else
(define M-state-if-else
  (lambda (cond then else state gotos class instance)
    (if (M-bool cond state gotos class instance)
        (M-state (list then) state gotos class instance)
        (M-state (list else) state gotos class instance))))

; while loop
(define M-state-while
  (lambda (lis state gotos class instance)
    (call/cc
     (lambda (break)
       (M-state-while-loop (firstparam lis) (secondparam lis) state (gotos-set-break break gotos) class instance)))))

; helper for the while
(define M-state-while-loop
 (lambda (cond body state gotos class instance)
   (if (M-bool cond state gotos class instance)
       (M-state-while-loop cond body 
         (call/cc
            (lambda (continue)
               (M-state (list body) state (gotos-set-continue continue gotos) class instance))) gotos class instance)
         state)))

; continue statement in loops
(define M-state-continue
  (lambda (lis state gotos)
    ((gotos-get-continue gotos) (removeLayer state))))

; break statement in loops
(define M-state-break
  (lambda (state gotos)
    ((gotos-get-break gotos) (removeLayer state))))

; sets the value of return as a variable
(define M-state-return
  (lambda (lis state gotos class instance)
    ((gotos-get-return gotos) (M-set-return state lis gotos class instance))))  

;declares variable
(define M-state-declare
  (lambda (lis state gotos class instance)
    ; checks that variable has not been declared 
    (if (isDeclared state (cadr lis))
     (error "Variable has already been declared")
    (if(null? (cdr (cdr lis)))
       ;if just declaring
        (M-state-declare-var (cdr lis) state gotos)
        ;if declaring and initializing
        (M-state-declare-initialize (cadr lis) (caddr lis) state gotos class instance)))))

; assignment for classes, variables, instances, functions, etc
(define M-state-assign
  (lambda (lis state gotos class instance)
    (if (list? (firstparam lis))
        (begin (set-instance-var (caddr (firstparam lis))
                                 (secondparam lis)
                                 (cadr (M-object (cadr (firstparam lis)) state gotos class instance))
                                 gotos
                                 (car (M-object (cadr (firstparam lis)) state gotos class instance))
                                 state) state)
        (if (contains-var? (firstparam lis) state)
            (M-state-setVar state (firstparam lis) (secondparam lis) gotos class instance)
            (begin (set-instance-var (firstparam lis) (secondparam lis) instance class state) state)))))

; handles blocks of code
(define M-state-begin
 (lambda (lis state gotos class instance)
   (cond
     ((null? lis) (removeLayer state))
     (else (M-state-begin (cdr lis) (M-state (list (car lis)) state gotos class instance) gotos class instance)))))

; try block
(define M-state-try
  (lambda (lis state gotos class instance)
    (cond
      ((null? (thirdparam lis)) (M-state-try-catch (firstparam lis) (secondparam lis) state gotos class instance))
       (else (M-state-finally (thirdparam lis) (M-state-try-catch (firstparam lis) (secondparam lis) state gotos class instance) gotos class instance)))))

(define catch-var caadr)
(define catch-body caddr)

; try catch block
(define M-state-try-catch
  (lambda (body catch state gotos class instance)
     (call/cc
      (lambda (catchcc)
        (M-state body state (if (null? catch)
                                gotos
                                (gotos-set-throw (lambda (errorvalue state)
                                                   (catchcc (M-state-catch errorvalue (catch-var catch) (catch-body catch) state gotos class instance))) gotos)) class instance)))))
; finally statement
(define M-state-finally
  (lambda (body state gotos class instance)
    (removeLayer (M-state (cadr body) (addLayer state) gotos class instance))))

; catch block
(define M-state-catch
  (lambda (e-value e-param body state gotos class instance)
    (M-state body (M-state-declare (list 'var e-param e-value) (addLayer state) gotos class instance) gotos class instance)))

; throw statement
(define M-state-throw
  (lambda (e state gotos class instance)
    ((gotos-get-throw gotos) (M-value (cadr e) state gotos class instance) (removeLayer state))))

; instantiates a new instance
(define instantiate
  (lambda (true-type state)
    (new-instance true-type state)))

; gets the needed layer
(define needed-layer
  (lambda (state)
    (list (get-class-layer state))))

; evaluate a function
(define func-eval
  (lambda (func-name args state gotos class instance)
    (if (list? func-name)
        ((get-method (caddr func-name) (car (M-object (cadr func-name) state gotos class instance)) state)
         (map (lambda (arg) (M-value arg state gotos class instance)) args)
         (needed-layer state)
         (cadr (M-object (cadr func-name) state gotos class instance)))
        ((get-method func-name (get-instance-type instance) state)
         (map (lambda (arg) (M-value arg state gotos class instance)) args)
         (needed-layer state)
         instance))))

;; evaluates the value of a dot expression
;; correctly handles left and right side of dot calls
(define M-value-dot
  (lambda (left right state gotos class instance)
    (unbox (get-instance-var right (cadr (M-object left state gotos class instance))
                                (car (M-object left state gotos class instance)) state))))

; evaluates a value or an expression
(define M-value
  (lambda (value state gotos class instance)
    (cond
      ;value is a number
      ((number? value) value)
      ;value is #t or #f
      ((boolean? value) value)
      ((customboolean? value) (change-literal-boolean value))
      ((procedure? value) value);(func-eval (cadr value) (cddr value) state gotos))
      ;value contains a boolean operation
      ((list? value)
       (cond
         ((class-stmt? value) value)
         ((func-call-stmt? value) (func-eval (cadr value) (cddr value) state gotos class instance)) ; evaluate value of a function
         ((new-stmt? value) (instantiate (cadr value) state))
         ((dot-stmt? value) (M-value-dot (cadr value) (caddr value) state gotos class instance))
         ((isboolean? (operator value)) (M-bool value state gotos class instance))
         ;value contains an arithmetic operation
         ((arithmetic? (operator value)) (M-value-int state value gotos class instance))))
      
      ;declared, not initialized
      ((not (eq? (M-state-getVar state value) '())) (M-state-getVar state value))
      (else (error "Invalid expression")))))

; higher precedence bool operators
; for &&, ||, !
(define M-bool
  (lambda (condition state gotos class instance)
    (cond
      ((boolean? condition) condition)
      ((eq? condition 'true) #t)
      ((eq? condition 'false) #f)
      ((list? condition)
       (cond
         ((eq? '&& (operator condition)) (and (M-value (operand1 condition) state gotos class instance) (M-value (operand2 condition) state gotos class instance)))
         ((eq? '|| (operator condition)) (or (M-value (operand1 condition) state gotos class instance) (M-value (operand2 condition) state gotos class instance)))
         ((eq? '! (operator condition)) (not (M-value (operand1 condition) state gotos class instance)))
         (else (M-bool-comparison condition state gotos class instance))))
      ((not (eq? (M-state-getVar state condition) '())) (M-state-getVar state condition))
      ((eq? (M-state-getVar state condition) '()) (error "Variable not declared"))
      (else (error "Invalid expression")))))


; lower precedence bool operators
; ==, !=, >, <, >=, <= 
(define M-bool-comparison
  (lambda (expression state gotos class instance)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((list? expression)
       (cond
         ((eq? '== (operator expression)) (eq? (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '!= (operator expression)) (not (eq? (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance))))
         ((eq? '> (operator expression)) (> (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '< (operator expression)) (< (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '>= (operator expression)) (>= (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '<= (operator expression)) (<= (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((arithmetic? (operator expression)) (M-value-int state expression gotos class instance))
         ((isboolean? (operator expression)) (M-bool expression state gotos class instance))))
      ;if the variable has been declared 
      ((not (eq? (M-state-getVar state expression) '())) (M-state-getVar state expression))
      ((eq? (M-state-getVar state expression) '())
       (error "Variable not initialized"))
      (else (error "invalid expression")))))

; arithmetic
(define M-value-int
  (lambda (state expression gotos class instance)
    (cond
      ((number? expression) expression)
      ;checks if expression is a list that needs to be evaluated
      ((list? expression)
       (cond
         ((eq? '+ (operator expression)) (+ (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '- (operator expression)) (M-value-negate state expression gotos class instance))
         ((eq? '* (operator expression)) (* (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '/ (operator expression)) (quotient (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))
         ((eq? '% (operator expression)) (remainder (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))))
      ;if expression is a declared variable
      ((not (eq? (M-state-getVar state expression) '())) (M-state-getVar state expression))
      ;if expression is a variable that has not been declared
      ((eq? (M-state-getVar state expression) '())
       (error "Variable not initialized"))
      (else (error "illegal arithmetic expression ")))))

;helper method to handle cases such as x = -x vs- x = x-5
(define M-value-negate
  (lambda (state expression gotos class instance)
    (if (null? (cdr (cdr expression)))
        (* -1 (M-value (operand1 expression) state gotos class instance))
        (- (M-value (operand1 expression) state gotos class instance) (M-value (operand2 expression) state gotos class instance)))))


;;
; Deal with true/false <-> #t/#f
;;

(define customboolean?
  (lambda (value)
    (cond
      ((eq? 'true value) #t)
      ((eq? 'false value) #t)
      (else #f))))

(define change-literal-boolean
  (lambda (value)
    (if (eq? value 'true)
        #t
        #f)))

; returns if the line is arithmetic
(define arithmetic?
  (lambda (symbol)
    (cond
      ((eq? symbol '+) #t)
      ((eq? symbol '-) #t)
      ((eq? symbol '/) #t)
      ((eq? symbol '*) #t)
      ((eq? symbol '%) #t)
      (else #f))))

;returns true if the operation is a boolean operation
(define isboolean?
  (lambda (symbol)
    (cond
      ((eq? symbol '&&) #t)
      ((eq? symbol '||) #t)
      ((eq? symbol '!) #t)
      ((eq? symbol '==) #t)
      ((eq? symbol '!=) #t)
      ((eq? symbol '>) #t)
      ((eq? symbol '<) #t)
      ((eq? symbol '>=) #t)
      ((eq? symbol '<=) #t)
      (else #f))))

; redefines first value of postfix expression as operator
(define operator
  (lambda (expression)
    (car expression)))

; redefines second value of postfix expression as operand1
(define operand1
  (lambda (expression)
    (cadr expression)))

; redefines third value of postfix expression as operand2
(define operand2
  (lambda (expression)
    (caddr expression)))

; first parameter of an expression
(define firstparam
  (lambda (expression)
    (cadr expression)))

; second parameter of an expression
(define secondparam
  (lambda (expression)
    (caddr expression)))

; third parameter of an expression
(define thirdparam
  (lambda (expression)
    (cadddr expression)))

; second layer
(define secondlayer
  (lambda (expression)
    (cdr expression)))

; branch of a tree
(define branch car)

;;;;;;;;;;;;;;;;;;;;
;;;;;; GOTOS ;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(define gotos-get-break car)
(define gotos-get-continue cadr)
(define gotos-get-throw caddr)
(define gotos-get-return cadddr)

(define gotos-set-break
  (lambda (new-break gotos)
    (list
     new-break
     (gotos-get-continue gotos)
     (gotos-get-throw gotos)
     (gotos-get-return gotos))))

(define gotos-set-continue
  (lambda (new-continue gotos)
    (list
     (gotos-get-break gotos)
     new-continue
     (gotos-get-throw gotos)
     (gotos-get-return gotos))))


(define gotos-set-throw
  (lambda (new-throw gotos)
    (list
     (gotos-get-throw gotos)
     (gotos-get-continue gotos)
     new-throw
     (gotos-get-return gotos))))

(define gotos-set-return
  (lambda (new-return gotos)
    (list
     (gotos-get-break gotos)
     (gotos-get-continue gotos)
     (gotos-get-throw gotos)
     new-return)))

(define gotos-init
  (list
   (lambda (state) (error 'goto-error "Break goto has not been set"))
   (lambda (state) (error 'goto-error "Continue goto has not been set"))
   (lambda (value state) (error 'goto-error "Throw goto has not been set"))
   (lambda (value state) (error 'goto-error "Return goto has not been set"))))

;;;;;;;;;;;;;;;;;;;;;;;;
; STATEMENT CONDITIONS ;
;;;;;;;;;;;;;;;;;;;;;;;;

(define declaration-stmt?
  (lambda (branch)
    (eq? (operator branch) 'var)))

(define while-stmt?
  (lambda (branch)
    (eq? (operator branch) 'while)))

(define if-stmt?
  (lambda (branch)
    (eq? (operator branch) 'if)))

(define return-stmt?
  (lambda (branch)
    (eq? (operator branch) 'return)))

(define assign-stmt?
  (lambda (branch)
    (eq? (operator branch) '=)))

(define break-stmt?
  (lambda (branch)
    (eq? (operator branch) 'break)))

(define throw-stmt?
  (lambda (branch)
    (eq? (operator branch) 'throw)))

(define continue-stmt?
  (lambda (branch)
    (eq? (operator branch) 'continue)))

(define return-stmt?
  (lambda (branch)
    (eq? (operator branch) 'return)))

(define begin-stmt?
  (lambda (branch)
    (eq? (operator branch) 'begin)))

(define try-stmt?
  (lambda (branch)
    (eq? (operator branch) 'try)))

(define func-def-stmt?
  (lambda (branch)
    (or (eq? (operator branch) 'function) (eq? (operator branch) 'static-function))))

(define func-call-stmt?
  (lambda (branch)
    (eq? (operator branch) 'funcall)))

(define dot-stmt?
  (lambda (branch)
    (eq? (operator branch) 'dot)))

(define new-stmt?
  (lambda (branch)
    (eq? (operator branch) 'new)))

(define class-stmt?
  (lambda (branch)
    (eq? (operator branch) 'class)))