(set-option :produce-models true)

(define-sort FLT () (_ FloatingPoint 11 53))

; Variables
(declare-fun x0 () FLT)
(declare-fun x1 () FLT)
(declare-fun x2 () FLT)

; Bounds
(define-fun a () FLT ((_ to_fp 11 53) RNE 1.0))
(define-fun b () FLT ((_ to_fp 11 53) RNE 2.0))

(assert (and 
	(fp.leq a x0) (fp.leq x0 b)
	(fp.leq a x1) (fp.leq x1 b)
	(fp.leq a x2) (fp.leq x2 b)))

; Functions
(define-fun p0 () FLT (fp.sub RNE (fp.add RNE x0 x1) x2))
(define-fun p1 () FLT (fp.sub RNE (fp.add RNE x1 x2) x0))
(define-fun p2 () FLT (fp.sub RNE (fp.add RNE x2 x0) x1))
(define-fun sum () FLT (fp.add RNE (fp.add RNE p0 p1) p2))

; Bounds of the function
(assert (or
	(fp.lt sum ((_ to_fp 11 53) RNE 3.0))
	(fp.gt sum ((_ to_fp 11 53) RNE 6.0))))

(check-sat)
(get-model)
(exit)

