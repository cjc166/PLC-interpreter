(load "classParser.scm")
(load "state.scm")
(load "projectOne.scm")

;; abstract the main function call
(define main-function '(funcall main))

;; the initial state
(define M-state-init '((()())))

;; no parameters yet
(define no-params-yet '())

;; function designed to interpret a file and a class of that file
;; takes in a filename and a classname to interpret
;; initializes all the classes, the top level only handling the class definitions
;; interprets the class and runs the main method of the class specified
(define interpret
  (lambda (filename classname)
    ((lambda (state)
       ((M-state-getVar (get-method-layer (get-class (string->symbol classname) (get-class-layer state))) 'main)
        no-params-yet state (get-class (string->symbol classname) (get-class-layer state))))
       (initialize-classes (parser filename) M-state-init gotos-init no-params-yet no-params-yet))))