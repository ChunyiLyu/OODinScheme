(define-class mobile (object) left right)
(define-class branch (object) length structure)

(define-method left-branch mobile
  (lambda (self) left))
(define-method right-branch mobile
  (lambda (self) right))
(define-method branch-length branch
  (lambda (self) length))
(define-method branch-structure branch
  (lambda (self) structure))

(define-method total-weight mobile
  (lambda (self)
  (let ((left-b (tell self left-branch))
        (right-b (tell self right-branch)))
    ((tell left-b branch-weight)
      +
      (tell right-b branch-weight)))))

(define-method branch-weight branch
  (lambda (self)
  (let ((structure (tell self branch-structure)))
    (if (number? structure)
        then structure
        else (tell self total-weight)))))

(define-method branch-torque branch
  (lambda (self)
    ((tell self branch-length)
      *
      (tell self branch-weight))))

(define-method branch-balanced? branch
  (lambda (self)
    (let ((structure (tell self branch-structure)))
      (or (number? structure)
          (tell structure balanced?)))))

(define-method balanced? mobile
  (lambda (self)
  (let ((left  (tell self left-branch))
        (right (tell self right-branch)))
      (and (tell left branch-balanced?)
           (tell right branch-balanced?)
           ((tell left branch-torque)
             =
             (tell right branch-torque))))))

(define r-branch0 (make branch (length 10) (structure 1)))
(define r-branch1 (make branch (length 0) (structure 10)))
(define l-branch0 (make branch (length 2) (structure 5)))
(define balanced (make mobile (left l-branch0) (right r-branch0)))
(define unbalanced (make mobile (left l-branch0) (right r-branch1)))

(tell balanced total-weight)
(tell balanced balanced?)
(tell unbalanced balanced?)
