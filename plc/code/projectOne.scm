; GROUP 15 INTERPRETER PART ONE
; Caleb Cain, Grace Dai, Savita Medlang

; Parses file, loads each line into interpreter
; determines the correct return value


;FOR INTERPRETER PT 2:
; ADD A CONDITION FOR BEGIN IN M-STATE
; then, have a function for begin, which will continuous call
; Mstate on each list in the block ((car lis))

;ADD A CONDITION FOR BREAK


;ADD A CONDITION FOR CONTINUE

(load "simpleParser.scm")
(load "state.scm")

;interpret
;takes the html file
(define interpret
  (lambda (filename)
      (M-value-expression (parser filename) M-state-init)))

; one statement at a time
(define M-value-expression
  (lambda (lis state)
    (M-get-return (M-state-stmt-list lis state))))

;
(define M-state-stmt-list
  (lambda (slist s)
    (if (null? slist)
        s
        (M-state-stmt-list (cdr slist) (M-state (car slist) s)))))

;evaluates each line
(define M-state
  (lambda (lis state)
    (cond
      ((eq? (car lis) 'var) (M-state-declare lis state))
      ((eq? (car lis) 'while) (M-state-while (cadr lis) (caddr lis) state));while
      ((eq? (car lis) 'if) (M-state-if lis state));if
      ((eq? (car lis) 'return) (M-state-return state lis))
      ((eq? (car lis) '=) (M-state-assign lis state))
      (else 'badoperator "illegal expression"))))

; line by line
(define M-state-stmt
  (lambda (stmt state)
    (cond
      ((number? stmt) stmt)
      ((arithmetic? (car stmt)) (M-value-int state stmt))
      ((isboolean? (car stmt)) (M-value-bool state stmt))
      (else M-state stmt state ))))

; evaluates if condition
(define M-state-if
  (lambda (lis state)
    (if (null? (cdr (cdr (cdr lis))))
        (M-state-if-no-else (cadr lis) (caddr lis) state)
        (M-state-if-else (cadr lis) (caddr lis) (cadddr lis) state))))

; if condition, no else
(define M-state-if-no-else
  (lambda (cond then state)
    (if (M-bool cond state)
        (M-state then state)
        state)))

; if statement with else
(define M-state-if-else
  (lambda (cond then else state)
    (if (M-bool cond state)
        (M-state then state)
        (M-state else state))))

; while loop
(define M-state-while
  (lambda (cond body state)
    (if (M-bool cond state)
        (M-state-while cond body (M-state body state))
         state)))

; higher precedence bool operators
; for &&, ||, !
(define M-bool
  (lambda (condition state)
    (cond
      ((boolean? condition) condition)
      ((eq? condition 'true) #t)
      ((eq? condition 'false) #f)
      ((list? condition)
       (cond
         ((eq? '&& (operator condition)) (and (M-bool (operand1 condition) state) (M-bool (operand2 condition) state)))
         ((eq? '|| (operator condition)) (or (M-bool (operand1 condition) state) (M-bool (operand2 condition) state)))
         ((eq? '! (operator condition)) (not (M-bool (operand1 condition) state)))
         (else (M-bool-comparison condition state))))
      ((not (eq? (M-state-getVar state condition) '())) (M-state-getVar state condition))
      ((eq? (M-state-getVar state condition) '()) (error "Variable not declared"))
      (else (error "Invalid expression")))))


; lower precedence bool operators
; ==, !=, >, <, >=, <= 
(define M-bool-comparison
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((boolean? expression) expression)
      ((list? expression)
       (cond
         ((eq? '== (operator expression)) (eq? (M-bool-comparison (operand1 expression) state) (M-bool-comparison (operand2 expression) state)))
         ((eq? '!= (operator expression)) (not (eq? (M-bool-comparison (operand1 expression) state) (M-bool-comparison (operand2 expression) state))))
         ((eq? '> (operator expression)) (> (M-bool-comparison (operand1 expression) state) (M-bool-comparison (operand2 expression) state)))
         ((eq? '< (operator expression)) (< (M-bool-comparison (operand1 expression) state) (M-bool-comparison (operand2 expression) state)))
         ((eq? '>= (operator expression)) (>= (M-bool-comparison (operand1 expression) state) (M-bool-comparison (operand2 expression) state)))
         ((eq? '<= (operator expression)) (<= (M-bool-comparison (operand1 expression) state) (M-bool-comparison (operand2 expression) state)))
         ((arithmetic? (operator expression)) (M-value-int state expression))
         ((isboolean? (operator expression)) (M-bool expression state))))
      ;if the variable has been declared 
      ((not (eq? (M-state-getVar state expression) '())) (M-state-getVar state expression))
      ((eq? (M-state-getVar state expression) '())
       (error "Variable not initialized"))
      (else (error "invalid expression")))))

; evaluates a value or an expression
(define M-value
  (lambda (value state)
    (cond
      ;value is a number
      ((number? value) value)
      ;value is #t or #f
      ((boolean? value) value)
      ;value contains a boolean operation
      ((list? value)
       (cond
         ((isboolean? (operator value)) (M-bool value state))
         ;value contains an arithmetic operation
         ((arithmetic? (operator value)) (M-value-int state value))))
      
      ;declared, not initialized
      ((not (eq? (M-state-getVar state value) '())) (M-state-getVar state value))
      (else (error "Invalid expression")))))

; arithmetic
(define M-value-int
  (lambda (state expression)
    (cond
      ((number? expression) expression)
      ;checks if expression is a list that needs to be evaluated
      ((list? expression)
       (cond
         ((eq? '+ (operator expression)) (+ (M-value-int state (operand1 expression)) (M-value-int state (operand2 expression))))
         ((eq? '- (operator expression)) (M-value-negate state expression))
         ((eq? '* (operator expression)) (* (M-value-int state (operand1 expression)) (M-value-int state (operand2 expression))))
         ((eq? '/ (operator expression)) (quotient (M-value-int state (operand1 expression)) (M-value-int state (operand2 expression))))
         ((eq? '% (operator expression)) (remainder (M-value-int state (operand1 expression)) (M-value-int state (operand2 expression))))))
      ;if expression is a declared variable
      ((not (eq? (M-state-getVar state expression) '())) (M-state-getVar state expression))
      ;if expression is a variable that has not been declared
      ((eq? (M-state-getVar state expression) '())
       (error "Variable not initialized"))
      (else (error "illegal arithmetic expression ")))))

;helper method to handle cases such as x = -x vs- x = x-5
(define M-value-negate
  (lambda (state expression)
    (if (null? (cdr (cdr expression)))
        (* -1 (M-value-int state (operand1 expression)))
        (- (M-value-int state (operand1 expression)) (M-value-int state (operand2 expression))))))

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


;declares variable
(define M-state-declare
  (lambda (lis state)
    ; checks that variable has not been declared 
    (if (> (indexof (cadr lis) (declared state)) -1)
     (error "Variable has already been declared")
    (if(null? (cdr (cdr lis)))
       ;if just declaring
        (M-state-declare-var (cdr lis) state )
        ;if declaring and initializing
        (M-state-declare-initialize (cadr lis) (caddr lis) state)))))

; assign should take a list and state
; assigns value to a variable
(define M-state-assign
  (lambda (lis state)
    (M-state-setVar state (cadr lis) (caddr lis) )))

; sets the value of return as a variable
(define M-state-return
  (lambda (state lis)
    (M-set-return state lis)))

