(load "simpleParser.scm")
(load "state.scm")

;interpret
;takes the html file
(define interpret
  (lambda (filename)
      (M.value.expression (parser filename) (M.state.init))))

; one statement at a time
(define M.value.expression
  (lambda (lis state)
    (M.get.return (M.state.stmt.list lis state))))

;
(define M.state.stmt.list
  (lambda (slist s)
    (if (null? slist)
        s
        (M.state.stmt.list (cdr slist) (M.state (car slist) s)))))

;evaluates each line
(define M.state
  (lambda (lis state)
    (cond
      ((eq? (car lis) 'var) (M.state.declare lis state))
      ((eq? (car lis) 'while) (M.state.while (cadr lis) (caddr lis) state));while
      ((eq? (car lis) 'if) (M.state.if (cadr lis) (caddr lis) (cadddr lis) state));if
      ((eq? (car lis) 'return) (M.state.return state lis))
      ((eq? (car lis) '=) (M.state.assign lis state))
      (else 'badoperator "illegal expression"))))

(define M.state.stmt
  (lambda (stmt state)
    (cond
      ((number? stmt) stmt)
      ((arithmetic? (car stmt)) (M.value.int state stmt))
      (else M.state stmt state ))))
      


; if statement
(define M.state.if
  (lambda (cond then else state)
    (if (M.bool cond state)
        (M.state then state)
        (M.state else state))))

; while loop
(define M.state.while
  (lambda (cond body state)
    (if (M.bool cond state)
        (M.state.while cond body state)
         state)))

; for &&, ||, !
(define M.bool
  (lambda (condition state)
    (cond
      ((boolean? condition) condition)
      ((eq? '&& (operator condition)) (and (M.bool (operand1 condition) state) (M.bool (operand2 condition) state)))
      ((eq? '|| (operator condition)) (or (M.bool (operand1 condition) state) (M.bool (operand2 condition) state)))
      ((eq? '! (operator condition)) (not (eq? (M.bool (operand1 condition) state) (M.bool (operand2 condition) state))))
      (else (M.bool.comparison condition state)))))

; ==, !=, >, <, >=, <= 
(define M.bool.comparison
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((list? expression)
       (cond
         ((eq? '== (operator expression)) (eq? (M.bool.comparison (operand1 expression) state) (M.bool.comparison (operand2 expression) state)))
         ((eq? '!= (operator expression)) (not (eq? (M.bool.comparison (operand1 expression) state) (M.bool.comparison (operand2 expression) state))))
         ((eq? '> (operator expression)) (> (M.bool.comparison (operand1 expression) state) (M.bool.comparison (operand2 expression) state)))
         ((eq? '< (operator expression)) (< (M.bool.comparison (operand1 expression) state) (M.bool.comparison (operand2 expression) state)))
         ((eq? '>= (operator expression)) (>= (M.bool.comparison (operand1 expression) state) (M.bool.comparison (operand2 expression) state)))
         ((eq? '<= (operator expression)) (<= (M.bool.comparison (operand1 expression) state) (M.bool.comparison (operand2 expression) state)))
         ((arithmetic? (operator expression)) (M.value.int state expression))))
      ((not (eq? (M.state.getVar state expression) #f)) (M.state.getVar state expression))
      (else --1))))

; arithmetic
(define M.value.int
  (lambda (state expression)
    (cond
      ((number? expression) expression)
      ((list? expression)
       (cond
         ((eq? '+ (operator expression)) (+ (M.value.int state (operand1 expression)) (M.value.int state (operand2 expression))))
         ((eq? '- (operator expression)) (- (M.value.int state (operand1 expression)) (M.value.int state (operand2 expression))))
         ((eq? '* (operator expression)) (* (M.value.int state (operand1 expression)) (M.value.int state (operand2 expression))))
         ((eq? '/ (operator expression)) (quotient (M.value.int state (operand1 expression)) (M.value.int state (operand2 expression))))
         ((eq? '% (operator expression)) (remainder (M.value.int state (operand1 expression)) (M.value.int state (operand2 expression))))))
      ((not (eq? (M.state.getVar state expression) #f)) (M.state.getVar state expression))
      (else (error 'badoperator "illegal arithmetic expression")))))

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

(define operator
  (lambda (expression)
    (car expression)))

(define operand1 cadr)

(define operand2 caddr)


; declare should take a list and state
(define M.state.declare
  (lambda (lis state)
    (if (null? (cdr (cdr lis)))
        (M.state.declare.var (cdr lis) state )
        (M.state.declare.initialize (cadr lis) (caddr lis) state))))

; assign should take a list and state
(define M.state.assign
  (lambda (lis state)
    (M.state.setVar state (cadr lis) (caddr lis))))

(define M.state.return
  (lambda (state lis)
    (M.set.return state lis)))
; assign calls setVar
; setVar checks if variable is declared
; throw an error


