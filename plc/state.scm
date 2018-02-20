; Caleb Cain, Grace Dai, Savita Medlang
; State Abstraction


(define declared car)
(define assignments cadr)
(define M.state.init '((return) ('())))

;declared will contain variables that have been declared
;assignments will contain the values of each variable
;if the variable has not been initialized, then the value will be '()

; sets the variable return
(define M.set.return
  (lambda (state lis)
    (M.state.setVar state (car lis) (cadr lis))))

;gets the variable return
(define M.get.return
  (lambda (state)
    (cond
      ((eq? (M.state.getVar state 'return) #f) 'false)
      ((eq? (M.state.getVar state 'return) #t) 'true)
      (else (M.state.getVar state 'return)))))
    

;find the index
;return the value
;getvar

(define M.state.getVar
  (lambda (state variable)
    (cond
      ;if variable has been declared
      ((> (indexof variable (declared state)) -1)
       ;then return the value of the index of the variable in assignments
       (getIndexValue (indexof variable (declared state)) (assignments state)))
      ; throws error, variable has not declared 
      (else (error "Using variable before declaring")))))
         

; updates the value of a variable, returns the entire updated state
(define M.state.setVar
  (lambda (state variable value)

    ;then update its value in the assignments list
    (cond
      ;if variable has been declared
      ((> (indexof variable (declared state)) -1)
       ;then replace the assigned value with value and return the new state
       ;setindex (state index newvalue)
       (cons (declared state) (cons (setIndexValue (assignments state) (indexof variable (declared state)) (M.value value state)) '())))
      
      ; else variable has not been declared
      ;and throw error

      (else (error "Using variable before declaring")))))
    
;declare variable
; e.g. var x;
(define M.state.declare.var
  (lambda (var state)
    (cons (myappend var (declared state)) (cons (cons '() (assignments state)) '()))))

;declare and initialize
; e.g. var x = 5;
(define M.state.declare.initialize
  (lambda (var value state)
    (cons (cons var (declared state)) (cons (cons (M.value value state) (assignments state)) '()))))

;HELPER METHODS FOR INDEXING THRU STATE


(define indexof-helper
  (lambda (x lis break)
    (cond
      ((null? lis) (break -1))
      ((equal? (car lis) x) 0)
      (else (+ 1 (indexof-helper x (cdr lis) break))))))

;finds the index of a variable in the state
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
;sets value at inputed index
(define setIndexValue
  (lambda (astate index value)
    (cond
      ((eq? 0 index) (cons value (cdr astate)))
      (else (cons (car astate) (setIndexValue (cdr astate) (- index 1) value))))))

;appends two lists together 
(define myappend
  (lambda (lis lis2)
    (cond
      ((null? lis) lis2)
      ((null? lis2) lis)
      (else (cons (car lis) (myappend(cdr lis) lis2))))))

