(define-class cat (object) age breed)
(define-method age? cat (lambda (self) age))
(define-method breed? cat (lambda (self) breed))

(define-class street-cat (cat) vaccines)
(define-class house-cat (cat) address)
(define-method address? house-cat (lambda (self) address))
(define-method vaccines? street-cat (lambda (self) vaccines))

(define-class kitty-cat (house-cat street-cat) weeks)
(define-method weeks? kitty-cat (lambda (self) weeks))

(define tiger (make cat (breed "persian") (age 7)))
(define Kiki (make kitty-cat (age 0) (breed "maine coon") (vaccines true) (address "hill house") (weeks 3)))

(set Kiki weeks 8)
(tell Kiki weeks?)
(tell Kiki age?)
(tell Kiki vaccines?)
(tell Kiki address?)
