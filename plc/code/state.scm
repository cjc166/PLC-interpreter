; Caleb Cain, Grace Dai, Savita Medlang
; State Abstraction

(load "projectOne.scm")


(define declared car)
(define assignments cadr)
(define M-state-init '((return) ('())))

(define current
  (lambda (state)
    (cond
      ((null? state) '())
      (else (car state)))))

;declared will contain variables that have been declared
;assignments will contain the values of each variable
;if the variable has not been initialized, then the value will be '()

; sets the variable return
(define M-set-return
  (lambda (state lis)
    (M-state-setVar state (car lis) (cadr lis))))

;gets the variable return
(define M-get-return
  (lambda (state)
    (cond
      ((eq? (M-state-getVar state 'return) #f) 'false)
      ((eq? (M-state-getVar state 'return) #t) 'true)
      (else (M-state-getVar state 'return)))))
    

;find the index
;return the value
;getvar

(define M-state-getVar
  (lambda (state variable)
    (cond
      ((null? state) (error "Using before declaring"))
      
      ;if variable has been found in the current state list
      ((eq? (isDeclared state variable) #t)
       
       ;then return the value of the index of the variable in assignments
       (getIndexValue (indexof variable  (declared (current state))) (assignments (current state))))
      
      
      (else (M-state-getVar (cdr state) variable)))))
         

; updates the value of a variable, returns the entire updated state
(define M-state-setVar
  (lambda (state variable value)

    ;then update its value in the assignments list
    (cond
      
      ((null? state) (error "Using variable before declaring"))
      ;if variable has been declared
      ((eq? (isDeclared state variable) #t)
       ;then replace the assigned value with value and return the new state
       ;setindex (state index newvalue)
       (cond 
         ((null? (cdr state)) 
           (cons (declared (current state)) (cons (setIndexValue (assignments (current state))
                                                                 (indexof variable (declared(current state))) (M-value value state)) '())))
           
         (else(cons (cons (declared (current state)) (cons (setIndexValue (assignments (current state))
                                                                 (indexof variable (declared(current state))) (M-value value state)) '())) (cdr state)))))
       
       (else (cons (car state) (cons (M-state-setVar (cdr state) variable value) '()))))))
      
;check if variable has been declared
(define isDeclared
  (lambda (state variable)
    (cond
       ((> (indexof variable (current(declared state))) -1) #t)
       (else #f))))

;declare variable
; e.g. var x;
(define M-state-declare-var
  (lambda (var state)
    (cons (myappend var (current(declared state))) (cons (cons '() (current(assignments state))) '()))))

;declare and initialize
; e-g- var x = 5;
(define M-state-declare-initialize
  (lambda (var value state)
    (cons (cons var (current(declared state))) (cons (cons (M-value value state) (current(assignments state))) '()))))


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


; add layer to the state at the beginning of the list
(define addLayer
  (lambda (state)
    (cons '(()()) state)))

; removes the first layer of the list (the most recently added layer)
(define removeLayer
  (lambda (state)
    (cdr state)))
