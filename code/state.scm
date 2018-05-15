; Caleb Cain, Grace Dai, Savita Medlang
; State Abstraction

;; represent the state as a list of variables and a list of their corresponding values
;; a class list value is represented as ('class parent-class properties methods)
;; each of the properties and methods are represented as a list of variables and their corresponding values
;; these lists look like ((var1 var2) (val1 val2)), for both properties and methods

;;;;;;;;;;;::;;;;;;;;;;;;;;;;
;;;;;;;; PART 4 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the abstractions for classes, parent class, properties and methods
(define no-parent-class 'null)
(define get-parent-class cadr)
(define get-property-layer caddr)
(define get-method-layer cadddr)

; get the class
(define get-class
  (lambda (class-name class-layer)
    (if (eq? 'null class-name)
        'null
        (M-state-getVar (list class-layer) class-name))))

; get the class layer
(define get-class-layer
  (lambda (environment)
    (cond
      ((null? (cdr environment)) (car environment))
      (else (get-class-layer (cdr environment))))))

; define a new class
(define new-class
  (lambda (parent-class-name parent-class property-layer method-layer)
    (if (eq? parent-class-name no-parent-class)
        (list 'class parent-class-name property-layer method-layer)
        (list 'class parent-class-name
              (merge-class-layers property-layer (get-property-layer parent-class))
              (merge-class-layers method-layer (get-method-layer parent-class))))))

; merge two class layers
; used for when a class extends another and inherits its properties and methods
(define merge-class-layers
  (lambda (player clayer)
    (list (list (append (caar player) (caar clayer)) (append (cadar player) (cadar clayer))))))

; searches class layers
(define search-class-layers
  (lambda (var layer)
    (remaining-indices (flatten (car layer)) var)))

; tells the remaining indices in a search for a variable so that we know how far in the list the variable is
(define remaining-indices
  (lambda (lis var)
    (cond
      ((null? lis) (error 'variable-not-found var lis "Variable not found"))
      ((eq? var (car lis)) (length (cdr lis)))
      (else (remaining-indices (cdr lis) var)))))

; search the super class layers
(define search-super-class-layers
  (lambda (var layer)
    (if (list? (car layer))
        (search-class-layers (cdr layer))
        (error 'super-class-search "No super class"))))

; gets a method
(define get-method
  (lambda (method-name class state)
    (M-state-getVar (get-method-layer (get-class class (get-class-layer state))) method-name)))

; abstractiongs to instances, the type and values indices
(define get-instance-type cadr)
(define get-instance-field-values caddr)
(define type-index 1)
(define values-index 2)

; defines a new instance
(define new-instance
  (lambda (true-type state)
    (list 'instance true-type (reverse* (map (lambda (val) (box (unbox val)))
                                             (cadar (get-property-layer (get-class true-type (get-class-layer state)))))))))
; the super instance of an instance
(define super-instance
  (lambda (instance class state)
    (if (eq? (length (super-properties class state))
             (length (get-instance-field-values instance)))
             instance
             (super-instance (set-instance-field-values instance (cdr (get-instance-field-values instance))) class state))))

; the super properties
(define super-properties
  (lambda (class state)
    (cadr (get-property-layer (get-class (get-parent-class (get-class class (get-class-layer state))) (get-class-layer state)))))) 

; sets the instance variable
(define set-instance-var
  (lambda (var val instance gotos class state)
    (set-box! (get-instance-var var instance class state) (M-value val state gotos class instance))))

; gets an instance variable
(define get-instance-var
  (lambda (var instance class state)
    (instance-get-index (flatten (get-instance-field-values instance))
                        (search-class-layers var (car (get-property-layer (get-class class (get-class-layer state))))))))

; gets index in an instance
(define instance-get-index
  (lambda (values index)
    (if (zero? index)
        (car values)
        (instance-get-index (cdr values) (- index 1)))))

; set type of an index
(define set-instance-type
  (lambda (instance type)
    (set-index type-index type instance)))

; set the fields
(define set-instance-field-values
  (lambda (instance values)
    (set-index values-index values instance)))

; flatten a list
(define flatten
  (lambda (lis)
    (cond
      ((null? lis) '())
      ((list? (car lis)) (append (flatten (car lis)) (flatten (cdr lis))))
      (else (cons (car lis) (flatten (cdr lis)))))))

; reverse a list
(define reverse*
  (lambda (lis)
    (reverse*-cps lis (lambda (v) v))))

; reverse a list
(define reverse*-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return lis))
      ((list? (car lis)) (reverse*-cps (cdr lis) (lambda (v) (reverse*-cps (car lis) (lambda (u) (return (append v (cons x '()))))))))
      (else (reverse*-cps (cdr lis) (lambda (v) (return (append v (cons (car lis) '())))))))))

; index in a list
(define set-index
  (lambda (index value lis)
    (if (zero? index)
        (cons value (cdr lis))
        (cons (car lis) (set-index (- index 1) value (cdr lis))))))

; abstraction for state
(define declared car)
(define assignments cadr)
(define M-state-init '((()())))

; the current list in a state
(define current
  (lambda (state)
    (cond
      ((null? state) '())
      (else (car state)))))

; sets the variable return
(define M-set-return
  (lambda (state lis gotos class instance)
    (M-state-setVar (M-state-declare-var (list (car lis)) state gotos) (car lis) (cadr lis) gotos class instance)))

;gets the variable return
(define M-get-return
  (lambda (state)
    (cond
      ((null? state) 'void)
      ((eq? (M-state-getVar state 'return) #f) 'false)
      ((eq? (M-state-getVar state 'return) #t) 'true)
      (else (M-state-getVar state 'return)))))

; gets the value of a variable from a state
(define M-state-getVar
  (lambda (state variable)
    (cond
      ((and (null? state) (eq? variable 'return)) 'void)
      ((null? state) (error "Using before declaring"))

      ;if variable has been found in the current state list
      ((eq? (isDeclared state variable) #t)
       
       ;then return the value of the index of the variable in assignments
       (if (box? (getIndexValue (if (pair? (car (car state)))
                                 (indexof variable  (declared (current state)))
                                 (indexof variable (declared state)))
                             (if (pair? (car (car state)))
                                 (assignments (current state))
                                 (assignments state)) ))
                 (unbox (getIndexValue (if (pair? (car (car state)))
                                 (indexof variable  (declared (current state)))
                                 (indexof variable (declared state)))
                             (if (pair? (car (car state)))
                                 (assignments (current state))
                                 (assignments state)) ))
                 (getIndexValue (if (pair? (car (car state)))
                                 (indexof variable  (declared (current state)))
                                 (indexof variable (declared state)))
                             (if (pair? (car (car state)))
                                 (assignments (current state))
                                 (assignments state)) )))
      (else (M-state-getVar (cdr state) variable)))))
         

; updates the value of a variable, returns the entire updated state
(define M-state-setVar
  (lambda (state variable value gotos class instance)

    ;then update its value in the assignments list
    (cond
      
      ((null? state) (error "Using variable before declaring"))
      ;if variable has been declared
      ((eq? (isDeclared state variable) #t)
       ;then replace the assigned value with value and return the new state
       ;setindex (state index newvalue)
       (cond 
         ((null? (cdr state)) 
           (cons (cons (declared (current state)) (cons (setIndexValue (assignments (current state))
                                                                 (indexof variable (declared (current state)))
                                                                 (begin (set-box! (lookup-box variable state) (M-value value state gotos class instance)) (lookup-box variable state)) variable) '())) '()))
           
         (else (cons (cons (declared (current state)) (cons (setIndexValue (assignments (current state))
                                                                 (indexof variable (declared(current state)))
                                                                 (begin (set-box! (lookup-box variable state) (M-value value state gotos class instance)) (lookup-box variable state)) variable) '())) (cdr state)))))
       
       ;(else (cons (car state) (cons (M-state-setVar (cdr state) variable value gotos) '()))))))
      (else (cons (car state) (M-state-setVar (cdr state) variable (lookup state value gotos class instance) gotos class instance))))))

; lookup a box of a variable
(define lookup-box
  (lambda (variable state)
    (cond
      ((null? state) (error "Using variable before declaring"))
      ((layer-contains-var? variable (declared (current state))) (lookup-box-in-layer variable (current state)))
      (else (lookup-box (variable (cdr state)))))))

; create a new layer with variables and their values
(define construct-layer
  (lambda (variables values)
    (list variables values)))

; lookup a box in a layer
(define lookup-box-in-layer
  (lambda (variable layer)
    (cond
      ((null? layer) (error "Variable not found"))
      ((eq? variable (declared (current layer))) (current (assignments layer)))
      (else (lookup-box-in-layer variable ((cdr (declared layer)) (cdr (assignments layer))))))))

; whether or not the layer contains the variable
(define layer-contains-var?
  (lambda (variable layer)
    (cond
      ((null? layer) #f)
      ((eq? (car layer) variable)#t)
      (else (layer-contains-var? variable (cdr layer))))))

; whether or not the state contains the variable
(define contains-var?
  (lambda (variable state)
    (cond
      ((null? state) #f)
      ((layer-contains-var? variable (declared (current state))) #t)
      (else (contains-var? variable (cdr state))))))


;check if variable has been declared
(define isDeclared
  (lambda (state variable)
    (if (pair? (car (car state)))
        (if (> (indexof variable (declared (current state))) -1)
            #t
            #f)
        (if (> (indexof variable (declared state)) -1)
            #t
            #f))))

; lookup a variable
(define lookup
  (lambda (state variable gotos class instance)
    (cond
      ((number? variable) variable)
      (else (M-value variable state gotos class instance)))))

;declare variable
; e.g. var x;
(define M-state-declare-var
  (lambda (var state gotos)
    (cond
      ((null? (cdr state))
       (cons (cons (myappend var (declared (current state))) (cons (cons (box '()) (assignments (current state))) '())) '()))
      (else (cons (cons (myappend var (declared (current state))) (cons (cons (box '()) (assignments (current state))) '())) (cdr state))))))

;declare and initialize
; e-g- var x = 5;
(define M-state-declare-initialize
  (lambda (var value state gotos class instance)
    (cond
      ((null? (cdr state))
       (cons (cons (myappend (list var) (declared (current state))) (cons (cons (box (M-value value state gotos class instance)) (assignments (current state))) '())) '()))
      (else (cons (cons (myappend (list var) (declared (current state))) (cons (cons (box (M-value value state gotos class instance)) (assignments (current state))) '())) (cdr state))))))


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
(define setIndexValue
  (lambda (astate index value variable)
    (cond
      ((eq? 0 index) (cons  value (cdr astate)))
      (else (cons (car astate) (setIndexValue (cdr astate) (- index 1) value variable))))))

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
