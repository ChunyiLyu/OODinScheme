;; Chunyi Lyu
;; Object-Oriented PASTA (OOP)
;; Conference Project for Principles of Programming Languages - Fall 2015
;; Referenced instructions from https://mitpress.mit.edu/sicp/psets/ps7oop/readme.html

#lang eopl
(require racket/base)  ;; so we can use the random function
;;-------------------------------------------------------------------
;; grammar
;;
;; <exp> ::= <number>
;;         | <string>
;;         | <variable>
;;         | (<exp> <op> <exp>)   where <op> is:  + - * / = != < >
;;         | (if <exp> then <exp> else <exp>)
;;         | (let ((<var> <exp>)*) <exp>)
;;         | (lambda (<param>*) <exp>)
;;         | (<exp> <exp>*)
;;         | (begin <exp>+)
;;         | (<var> := <exp>)
;;         | (define <var> <exp>)
;;         | (load <exp>)
;;         | (zcons <exp> <exp>)
;;         | (define-class class-name <super-class>+ slots*)
;;         | (define-method method-name class-name lambda-exp)
;;         | (make class-name (name/value)*)
;;         | (tell instance-name method-name args*)
;;         | (set instance-name slot-name value)
;;         | (zcons <exp> <exp>)
;;
;; * = "zero or more"
;; + = "one or more"
;;
;;-------------------------------------------------------------------
;; read-eval-print loop

(define start
  (lambda ()
    (begin
      (display "Welcome to the SLC Scheme Interpreter\n\n")
      (read-eval-print (make-initial-env)))))

(define read-eval-print
  (lambda (env)
    (begin
      (display "==> ")
      (let ((input (read)))
        (if (equal? input 'quit)
            (begin
              (display "Goodbye!")
              (newline))
	    (let ((value (m (parse input) env)))
	      (begin
		(display (make-printable value))
		(newline)
		(read-eval-print env))))))))

;;-------------------------------------------------------------------
;; syntactic extensions (macros)

(define or-expander
  (lambda (input)
    (let ((exps (cdr input)))
      (cond
        ((null? exps) 'false)
        ((null? (cdr exps)) (car exps))
        (else (list 'if (car exps)
                    'then 'true
                    'else (cons 'or (cdr exps))))))))

(define swap-expander
  (lambda (input)
    (let ((var1 (cadr input))
          (var2 (caddr input)))
      (list 'let (list (list 'temp var1))
            (list 'begin
                  (list var1 ':= var2)
                  (list var2 ':= 'temp))))))

(define and-expander
  (lambda (input)
    (let ((exps (cdr input)))
      (cond
        ((null? exps) 'true)
        ((null? (cdr exps)) (car exps))
        (else (list 'if (car exps)
                    'then (cons 'and (cdr exps))
                    'else 'false))))))

(define *macro-names*
  (list 'or 'and 'swap!))

(define *macro-expanders*
  (list or-expander and-expander swap-expander))

(define syntactic-sugar?
  (lambda (input)
    (and (list? input)
         (not (null? input))
         (member (car input) *macro-names*))))

(define expand-macro-once
  (lambda (input)
    (let ((expand (retrieve (car input) *macro-names* *macro-expanders*)))
      (expand input))))

;;-------------------------------------------------------------------
;; parser

(define literal?
  (lambda (x) (or (number? x) (string? x))))

(define infix-op?
  (lambda (x)
    (and (symbol? x) (member x '(+ - * / = != < >)))))

(define reserved-keyword?
  (lambda (x)
    (and (symbol? x) (member x '(if begin let lambda define load zcons update-instance)))))

(define parse
  (lambda (input)
    (cond
      ((literal? input) (list 'lit-exp input))
      ((symbol? input) (list 'var-exp input))

      ;; syntactic extensions (macros)
      ((syntactic-sugar? input) (parse (expand-macro-once input)))

      ;; (zcons <exp> <exp>)
      ((and (list? input) (= (length input) 3) (eq? (car input) 'zcons))
       (list 'zcons-exp (parse (cadr input)) (parse (caddr input))))

      ;; infix operators: (<exp> <op> <exp>)
      ((and (list? input) (= (length input) 3) (infix-op? (cadr input)))
       (list 'infix-exp
             (cadr input)
             (parse (car input))
             (parse (caddr input))))

      ;; (if <exp> then <exp> else <exp>)
      ((and (list? input) (= (length input) 6) (eq? (car input) 'if))
       (list 'if-exp
             (parse (cadr input))
             (parse (cadddr input))
             (parse (cadddr (cddr input)))))

      ;; (begin <exp>+)
      ((and (list? input) (> (length input) 1) (eq? (car input) 'begin))
       (list 'begin-exp (map parse (cdr input))))

      ;; (<var> := <exp>)
      ((and (list? input) (= (length input) 3) (eq? (cadr input) ':=))
       (list 'assign-exp (car input) (parse (caddr input))))

      ;; (define <var> <exp>)
      ((and (list? input) (= (length input) 3) (eq? (car input) 'define))
       (list 'define-exp (cadr input) (parse (caddr input))))

      ;; (load <exp>)
      ((and (list? input) (= (length input) 2) (eq? (car input) 'load))
       (list 'load-exp (parse (cadr input))))

      ;; (let ((<var> <exp>)*) <exp>)
      ((and (list? input) (= (length input) 3) (eq? (car input) 'let))
       (let ((bindings (cadr input))
	     (body (caddr input)))
         (let ((binding-vars (map car bindings))
               (binding-exps (map cadr bindings)))
           (list 'let-exp
                 binding-vars
                 (map parse binding-exps)
                 (parse body)))))

      ;; procedures: (lambda (<param>*) <exp>)
      ((and (list? input) (= (length input) 3) (eq? (car input) 'lambda))
       (list 'lambda-exp (cadr input) (parse (caddr input))))

      ;; (define-class classname superclass+ slots*)
      ((eq? (car input) 'define-class)
       (list 'define-class (cadr input) (map parse (caddr input)) (cdddr input)))

      ;; (define-method methodname class <exp>)
      ((eq? (car input) 'define-method)
       (list 'define-method (cadr input) (parse (caddr input)) (parse (cadddr input))))

      ;; (make class <exp>*)
      ((eq? (car input) 'make)
       (list 'make (parse (cadr input)) (map (lambda (x) (list (car x) (parse (cadr x)))) (cddr input))))

      ;; (tell instance method-name <exp>*)
      ((eq? (car input) 'tell)
       (if (> (length input) 3)
           (list 'tell-exp (cadr input) (parse (caddr input)) (map parse (cdddr input)))
           (list 'tell-exp (cadr input) (parse (caddr input)) '())))

      ;;(set instance slot-name slot-value)
      ((eq? (car input) 'set)
       (list 'set-exp (parse (cadr input)) (caddr input) (parse (cadddr input)) (cadr input)))
            
      ;; applications: (<exp> <exp>*)
      ((and (list? input)
            (not (null? input))
            (not (reserved-keyword? (car input))))
       (list 'app-exp (parse (car input)) (map parse (cdr input))))

      (else (eopl:error "bad concrete syntax:" input)))))

;;-------------------------------------------------------------------
;; interpreter

(define value
  (lambda (input)
    (m (parse input) (make-initial-env))))

(define m
  (lambda (exp env)
    (cond
      ((eq? (car exp) 'lit-exp) (cadr exp))
      ((eq? (car exp) 'var-exp) (lookup (cadr exp) env))

      ;; (zcons-exp <exp> <exp>)
      ((eq? (car exp) 'zcons-exp)
       (let ((exp1 (cadr exp))
             (exp2 (caddr exp)))
         (cons (freeze exp1 env) (freeze exp2 env))))

      ;; (infix-exp <op> <exp> <exp>)
      ((eq? (car exp) 'infix-exp)
       (let ((op (cadr exp))
             (exp1 (caddr exp))
             (exp2 (cadddr exp)))
	 (let ((val1 (m exp1 env))
	       (val2 (m exp2 env)))
	   (cond
	     ((eq? op '+) (+ val1 val2))
	     ((eq? op '-) (- val1 val2))
	     ((eq? op '*) (* val1 val2))
	     ((eq? op '/) (/ val1 val2))
	     ((eq? op '=) (equal? val1 val2))
	     ((eq? op '!=) (not (equal? val1 val2)))
	     ((eq? op '<) (< val1 val2))
	     ((eq? op '>) (> val1 val2))
	     (else (eopl:error "bad infix operator:" op))))))

      ;; (if-exp <exp> <exp> <exp>)
      ((eq? (car exp) 'if-exp)
       (let ((test-exp (cadr exp))
             (then-exp (caddr exp))
             (else-exp (cadddr exp)))
         (cond
           ((m test-exp env) (m then-exp env))
           (else (m else-exp env)))))

      ;; (begin-exp (<exp>+))
      ((eq? (car exp) 'begin-exp)
       (let ((exps (cadr exp)))
         (m-sequential exps env)))

      ;; (assign-exp <var> <exp>)
      ((eq? (car exp) 'assign-exp)
       (let ((var (cadr exp))
             (rhs-exp (caddr exp)))
         (let ((val (m rhs-exp env)))
           (let ((ref (lookup-ref var env)))
             (set-contents! ref val)
             'ok))))

      ;; (define-exp <var> <exp>)
      ((eq? (car exp) 'define-exp)
       (let ((var (cadr exp))
             (rhs-exp (caddr exp)))
         (let ((val (m rhs-exp env)))
           (let ((ref (lookup-ref-in-first-frame var env)))
             (set-contents! ref val)
             'ok))))

      ;; (load-exp <exp>)
      ((eq? (car exp) 'load-exp)
       (let ((filename (m (cadr exp) env)))
         (if (not (string? filename))
             (eopl:error "load: filename must be a string")
             (let ((port (open-input-file filename)))
               (load-loop port env)))))
      
      ;; (let-exp (<var>*) (<exp>*) <exp>)
      ((eq? (car exp) 'let-exp)
       (let ((binding-vars (cadr exp))
             (binding-exps (caddr exp))
             (body (cadddr exp)))
         (let ((binding-vals (map (lambda (x) (m x env)) binding-exps)))
           (let ((extended-env (extend binding-vars binding-vals env)))
             (m body extended-env)))))

      ;; (lambda-exp (<param>*) <exp>)
      ((eq? (car exp) 'lambda-exp)
       (let ((params (cadr exp))
             (body (caddr exp)))
         (lambda (vals)
           (let ((extended-env (extend params vals env)))
             (m body extended-env)))))

      ;; (tell-exp <object-name> <method-name> (<arg1> ....))
      ((eq? (car exp) 'tell-exp)
       (let ((object-name (parse (cadr exp)))
             (method-name (caddr exp))
             (args (cadddr exp)))
         (let* ((inst (m object-name env))
               (meth (m method-name env))
               (class (list-ref inst 1)))
           (if (and (instance? inst) (subclass? class (m (list-ref meth 2) env)))
               (let* ((slot-names (list-ref class 3))
                      (slot-values (list-ref inst 2))
                      (extend-env (extend slot-names slot-values env))
                      (vals (map (lambda (x) (m x env)) args))
                      (ref (lookup-ref-in-first-frame 'self env))
                      (inst-ref (lookup-ref-in-first-frame (cadr exp) env))
                      (return-value ((m (list-ref meth 3) extend-env) (cons inst vals))))
                 ;; evaluate the method in an extended environment
                 (if (and (list? return-value) (equal? (car return-value) 'update-instance))
                   (let ((return-value (cdr return-value)))
                     (set-contents! inst-ref return-value)
                     return-value)
                 return-value))
               (eopl:error "method could not apply to instance >>> TELL" method-name)))))
               

      ;; (app-exp <exp> (<exp>*))
      ((eq? (car exp) 'app-exp)
           (let ((operator (cadr exp))
                 (operands (caddr exp)))
             (let ((proc (m operator env))
                   (vals (map (lambda (x) (m x env)) operands)))
               (proc vals))))

      ;; (define-class classname supername <slots>*)
      ((eq? (car exp) 'define-class)
       (let ((superclasses (map (lambda (x) (m x env)) (list-ref exp 2)))) ;; evaluate the superclasses
         (if (not (classes? superclasses))
             (eopl:error "Unrecognized superclass -- MAKE-CLASS >> "
                         (list-ref exp 2))
             (let ((name (list-ref exp 1)) ;; get the name, which is the second element
                   (all-slots (collect-slots
                               (list-ref exp 3)
                               superclasses)))
               (let ((new-class
                      (make-class name superclasses all-slots))
                     (ref (lookup-ref-in-first-frame name env)))
                 (set-contents! ref new-class)
                 (list 'defined 'class: name))))))

      ;; (define-method methodname classname lambda-exp)
       ((eq? (car exp) 'define-method)
        (let ((class (m (list-ref exp 2) env)))
          (if (not (class? class))
              (eopl:error "Unrecognized superclass -- MAKE-CLASS >> "
                         (list-ref exp 2))
              (let ((ref (lookup-ref-in-first-frame (list-ref exp 1) env)))
                (set-contents! ref exp)
                (list 'defined 'method: (list-ref exp 1) 'for 'class (list-ref exp 2))))))

       ;; (make class (slot/<exp> pairs))
       ((eq? (car exp) 'make)
        (let ((class (m (list-ref exp 1) env)))
          (if (not (class? class))
              (eopl:error "Unrecognized class -- MAKE >> "
               (list-ref exp 1))
              (let ((slots (list-ref exp 2)))
                (let ((specified-slot-names (map (lambda (x) (car x)) slots))
                      (specified-slot-values
                       (map (lambda (s) (m (cadr s) env))
                      slots)))
                  (make-standard-instance
                   class
                   (make-instance-slots
                    specified-slot-names
                    specified-slot-values
                    (list-ref class 3))))))))
       
       ;; (set-exp instance slot-name slot-value instance-name)
       ((eq? (car exp) 'set-exp)
        (let* ((inst (m (list-ref exp 1) env))
              (slot-name (list-ref exp 2))
              (slot-value (m (list-ref exp 3) env))
              (inst-class (list-ref inst 1))
              (slot-names (list-ref inst-class 3))
              (slot-values (list-ref inst 2))
              (index (index-of slot-name slot-names)))
              (if (= index -1) (eopl:error "slot name does not exist -- SET >> " slot-name)
                  (let* ((new-values (replace-element slot-value slot-values index))
                        (new-inst (make-standard-instance inst-class new-values))
                        (ref (lookup-ref-in-first-frame (list-ref exp 4) env)))          
                    (set-contents! ref new-inst)
                    (cons 'update-instance new-inst)))))

    
      (else (eopl:error "bad abstract syntax:" exp)))))



;;--------------------------instance helper functions--------------------------
(define (make-instance-slots names values all-names)
  (map (lambda (name)
         (get-initial-slot-value name names values))
       all-names))

(define (get-initial-slot-value name names values)
  (cond ((null? names) undefined-value)
        ((eq? name (car names)) (car values))
        (else (get-initial-slot-value name
                                      (cdr names)
                                      (cdr values)))))

(define undefined-value '*undefined*)

(define replace-element
  (lambda (x ls n)
    (cond
      ((= n 0) (cons x (cdr ls)))
      (else (cons (car ls) (replace-element x (cdr ls) (- n 1)))))))

(define index-of
  (lambda (x ls)
    (cond
      ((null? ls) -1)
      ((equal? x (car ls)) 0)
      (else  (+ 1 (index-of x (cdr ls)))))))

;;--------------------------class helper functions--------------------------
(define (make-class name superclass slot-names)
  (let ((subsuming
         (cond
           ((null? superclass)'())
           ((null? (cdr superclass)) (cons (car superclass) (list-ref (car superclass) 2)))
           (else (collect-superclasses superclass)))))
    (list 'class name (remove-duplicates subsuming) slot-names)))

(define collect-superclasses
  (lambda (classes)
    (if (null? (cdr classes))
        (cons (car classes) (list-ref (car classes) 2))
        (append (list (car classes) (list-ref (car classes) 2)) (collect-superclasses (cdr classes))))))


;;--------------------------helper functions for inheritance--------------------------
(define collect-slots
  (lambda (slots superclasses)
   (let ((superclass-slots (if (null? (cdr superclasses))
                               (list-ref (car superclasses) 3)
                               (map (lambda (x) (list-ref x 3)) superclasses))))
     (remove-duplicates (append slots (flatten superclass-slots))))))

(define make-standard-instance
  (lambda (class slot-values)
    (list 'instance class slot-values)))

(define subclass?
  (lambda (class1 class2)
    (or (eq? class1 class2)
        (member? class2 (list-ref class1 2)))))


;;--------------------------type checking helper functions--------------------------
(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;; check whether a list of classes are all classes
(define (classes? exps)
  (if (null? (cdr exps)) (class? (car exps))
      (and (class? (car exps)) (classes? (cdr exps)))))
  
(define (class? exp)
  (tagged-list? exp 'class))

(define (instance? exp)
  (tagged-list? exp 'instance))

;; only flatten one layer of a nested list
(define (flatten-one x)
  (cond ((null? x) '())
        ((pair? x) (append (car x) (cdr x)))
        (else (list x))))

;; flatten a nested list
(define flatten
  (lambda (x) 
    (cond ((null? x) '())
          ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
          (else (list x)))))

(define member?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((equal? (car ls) x) #t)
      (else (member? x (cdr ls))))))

(define remove-duplicates
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((member? (car ls) (cdr ls))
       (remove-duplicates (cdr ls)))
      (else (cons (car ls) (remove-duplicates (cdr ls)))))))


;; evaluates a list of expressions in left to right order
(define m-sequential
  (lambda (exps env)
    (cond
      ((null? (cdr exps)) (m (car exps) env))
      (else (let ((ignore (m (car exps) env)))
              (m-sequential (cdr exps) env))))))

;; reads expressions from a file and evaluates them in sequence
(define load-loop
  (lambda (port env)
    (let ((input (read port)))
      (if (eof-object? input)
          (begin
            (close-input-port port)
            'ok)
          (begin
            (m (parse input) env)
            (load-loop port env))))))

;;-------------------------------------------------------------------
;; initial environment

(define make-empty-env
  (lambda ()
    '()))

(define make-initial-env
  (lambda ()
    (let ((bindings (make-initial-bindings)))
      (let ((variables (map car bindings))
            (values (map cadr bindings)))
	(extend variables values (make-empty-env))))))

(define make-initial-bindings
  (lambda ()
    (list (list 'a 1)
          (list 'b 2)
          (list 'c 3)
          (list 'd 4)
          (list 'pi 3.14159)
          (list 'true #t)
          (list 'false #f)
	  (list 'nil '())
          (list 'object (list 'class 'object '() '())) ; predefined class 'object'
	  ;; primitive functions:
          (list 'number? (lambda (vals) (number? (car vals))))
          (list 'square (lambda (vals) (* (car vals) (car vals))))
          (list 'cube (lambda (vals) (* (car vals) (car vals) (car vals))))
          (list 'power (lambda (vals) (expt (car vals) (cadr vals))))
	  (list 'quotient (lambda (vals) (quotient (car vals) (cadr vals))))
	  (list 'remainder (lambda (vals) (remainder (car vals) (cadr vals))))
	  (list 'list (lambda (vals) vals))
          (list 'list-ref (lambda (vals) (list-ref (car vals) (cadr vals))))
          (list 'cons (lambda (vals) (cons (car vals) (cadr vals))))
          (list 'car (lambda (vals) (force-value (car (car vals)))))
          (list 'cdr (lambda (vals) (force-value (cdr (car vals)))))
          (list 'null? (lambda (vals) (null? (car vals))))
	  (list 'random (lambda (vals) (random (car vals))))
          (list 'print (lambda (vals)
			 (display (make-printable (car vals)))
			 (newline)
			 'ok))
          )))

;;-------------------------------------------------------------------
;; representation of memory references as Scheme vectors

;; creates a new reference containing x
(define make-ref
  (lambda (x)
    (vector x)))

;; retrieves the contents of a reference
(define deref
  (lambda (ref)
    (vector-ref ref 0)))

;; changes the contents of a reference to new-val
(define set-contents!
  (lambda (ref new-val)
    (vector-set! ref 0 new-val)))

;;-------------------------------------------------------------------
;; representation of environments with references
;;
;; an environment is now a list of references containing frames:
;;
;; <environment> ::= ( #(<frame>) #(<frame>) ...)
;;
;; a frame now consists of a list of symbols and a list of references
;; containing the symbols' associated values:
;;
;; <frame> ::= ( (<symbol> <symbol> ...) ( #(<value>) #(<value>) ...) )

(define extend
  (lambda (new-syms new-vals old-env)
    (let ((new-refs (map make-ref new-vals)))
      (let ((new-frame (list new-syms new-refs)))
        (cons (make-ref new-frame) old-env)))))

(define first-frame-vars
  (lambda (env)
    (car (deref (car env)))))

(define first-frame-refs
  (lambda (env)
    (cadr (deref (car env)))))

(define retrieve
  (lambda (var frame-vars frame-refs)
    (cond
      ((eq? (car frame-vars) var) (car frame-refs))
      (else (retrieve var (cdr frame-vars) (cdr frame-refs))))))

;; returns the reference associated with symbol var in env
(define lookup-ref
  (lambda (var env)
    (cond
      ((null? env) (eopl:error "unbound variable" var))
      ((member var (first-frame-vars env))
       (retrieve var (first-frame-vars env) (first-frame-refs env)))
      (else (lookup-ref var (cdr env))))))

;; returns the value associated with symbol var in env
(define lookup
  (lambda (var env)
    (deref (lookup-ref var env))))

;; lookup-ref-in-first-frame searches for a reference associated with
;; var in the first frame of the given environment and returns the
;; reference if it is found; otherwise it adds a new reference for var
;; to the first frame, and returns the new reference. Used by define.

(define lookup-ref-in-first-frame
  (lambda (var env)
    (cond
      ((member var (first-frame-vars env))
       (retrieve var (first-frame-vars env) (first-frame-refs env)))
      (else (let ((ref (make-ref 'undefined)))
              (extend-first-frame! env var ref)
              ref)))))

;; extend-first-frame! replaces the first frame of the given
;; environment by a new extended frame containing new-var and new-ref
;; (in addition to the original variables and references).

(define extend-first-frame!
  (lambda (env new-var new-ref)
    (let ((new-frame (list (cons new-var (first-frame-vars env))
                           (cons new-ref (first-frame-refs env)))))
      (set-contents! (car env) new-frame))))

;;------------------------------------------------------------------
;; representation of frozen and thawed values
;;
;; a "frozen value" is a three-element Scheme vector containing an
;; unevaluated parsed expression and an environment
;;
;; <frozen> ::= #(frozen <parsed-exp> <env>)
;;
;; To evaluate a frozen value, we evaluate the parsed expression
;; using its saved environment and then remember the resulting value.
;; An evaluated frozen value is called a "thawed" value. Evaluating a
;; thawed value just returns the already-computed value. We also no
;; longer need the environment, so we replace it by 'none.
;;
;; <thawed> ::= #(thawed <computed-value> none)

(define freeze
  (lambda (exp env)
    (vector 'frozen exp env)))

(define frozen?
  (lambda (x)
    (and (vector? x) (eq? (vector-ref x 0) 'frozen))))

(define get-frozen-exp
  (lambda (frozen)
    (vector-ref frozen 1)))

(define get-frozen-env
  (lambda (frozen)
    (vector-ref frozen 2)))

(define thawed?
  (lambda (x)
    (and (vector? x) (eq? (vector-ref x 0) 'thawed))))

(define get-computed-value
  (lambda (thawed)
    (vector-ref thawed 1)))

;; force-value forces the evaluation of a frozen value, but leaves
;; ordinary values alone

(define force-value
  (lambda (x)
    (cond
      ((frozen? x)
       (let ((value (m (get-frozen-exp x) (get-frozen-env x))))
         (vector-set! x 0 'thawed)
         (vector-set! x 1 value)
         (vector-set! x 2 'none)
         value))
      ((thawed? x) (get-computed-value x))
      (else x))))

;; make-printable suppresses the internal details of frozen values (and
;; lists of frozen values), so they can be printed out in a nice way

(define make-printable
  (lambda (x)
    (cond
      ((frozen? x) '<frozen>)
      ((thawed? x) (make-printable (get-computed-value x)))
      ((pair? x) (cons (make-printable (car x)) (make-printable (cdr x))))
      (else x))))

