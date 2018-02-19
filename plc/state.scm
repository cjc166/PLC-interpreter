(define declared car)
(define assignments cadr)
(define M.state.init '((return) (#f)))

;declared will contain variables that have been declared
;assignments will contain the values of each variable
;if the variable has not been initialized, then the value will be #f


(define M.set.return
  (lambda (state lis)
    (M.state.setVar state (car lis) (cadr lis))))

(define M.get.return
  (lambda (state)
    (M.state.getVar state 'return)))
    

;input state variable
;find the index
;return the value
;getvar

;if the variable has been declared, return its value
(define M.state.getVar
  (lambda (state variable)
    (cond
      ;if variable has been declared
      ((> (indexof variable (declared state)) -1)
       ;then return the value of the index of the variable in assignments
       (getIndexValue (indexof variable (declared state)) (assignments state)))
      (else #f))))
         

;return
;input state lis



; find the index
; update the variable
; return the state
(define M.state.setVar
  (lambda (state variable value)

    ;then update its value in the assignments list
    (cond
      ;if variable has been declared
      ((> (indexof variable (declared state)) -1)
       ;then replace the assigned value with value and return the new state
       ;setindex (state index newvalue)
       (cons (declared state) (cons (setIndexValue (assignments state) (indexof variable (declared state)) (M.value.int state value)) '())))
      
      ; else variable has not been declared
      ;and throw error

      (else (error 'badoperator "variable not found")))))
    

;input var
;cons the list to the variable
;declare
(define M.state.declare.var
  (lambda (var state)
    (cons (myappend var (declared state)) (cons (cons #f (assignments state)) '()))))

(define M.state.declare.initialize
  (lambda (var value state)
    (cons (cons var (declared state)) (cons (cons (M.value.int state value) (assignments state)) '()))))
  
;assignment
;input variable and value


;HELPER METHODS FOR INDEXING

(define indexof-helper
  (lambda (x lis break)
    (cond
      ((null? lis) (break -1))
      ((equal? (car lis) x) 0)
      (else (+ 1 (indexof-helper x (cdr lis) break))))))

(define indexof
  (lambda (x lis)
    (call/cc
     (lambda (break)
       (indexof-helper x lis break)))))


;gets the value of index in lis
(define getIndexValue
  (lambda (index lis)
    (cond
      ((= 0 index) (car lis))
      (else (getIndexValue (- index 1) (cdr lis))))))

;setindex
;input: state, index, and new value
(define setIndexValue
  (lambda (astate index value)
    (cond
      ((eq? 0 index) (cons value (cdr astate)))
      (else (cons (car astate) (setIndexValue (cdr astate) (- index 1) value))))))

(define myappend
  (lambda (lis lis2)
    (cond
      ((null? lis) lis2)
      ((null? lis2) lis)
      (else (cons (car lis) (myappend(cdr lis) lis2))))))

