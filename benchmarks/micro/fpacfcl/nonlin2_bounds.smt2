(set-option :produce-models true)

(define-sort FLT () (_ FloatingPoint 11 53))

; Variables and constants
(declare-fun x () FLT)
(declare-fun y () FLT)

(define-fun one () FLT ((_ to_fp 11 53) RNE 1.0))

; Bounds
(define-fun a () FLT ((_ to_fp 11 53) RNE 1.001))
(define-fun b () FLT ((_ to_fp 11 53) RNE 2.0))

(assert (and 
	(fp.leq a x) (fp.leq x b)
	(fp.leq a y) (fp.leq y b)))

; Functions
(define-fun t () FLT (fp.mul RNE x y))
(define-fun r () FLT
	    (fp.div RNE
	    	    (fp.sub RNE t one)
		    (fp.sub RNE (fp.mul RNE t t) one)))

; Bounds of the function
(assert (or
	(fp.lt r ((_ to_fp 11 53) RNE 0.2))
	(fp.gt r ((_ to_fp 11 53) RNE 0.5))))

(check-sat)
(get-model)
(exit)

