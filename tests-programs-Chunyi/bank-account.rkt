(define-class bank-account (object) balance)
(define-method inquiry bank-account
  (lambda (self) balance))
(define-method withdraw bank-account
  (lambda (self x)
    (if (x > balance)
      then "Insufficient funds"
      else (set self balance (balance - x)))))
(define-method deposit bank-account
  (lambda (self x)
    (set self balance (balance + x))))

(define account0 (make bank-account (balance 100)))

(tell account0 inquiry)
(tell account0 withdraw 20)
(tell account0 inquiry)
(tell account0 deposit 60)
(tell account0 inquiry)
