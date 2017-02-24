(set-option :produce-models true)

(define-sort FLT () (_ FloatingPoint 11 53))
(define-sort EXT_FLT () (_ FloatingPoint 11 56))

; Epsilon
(define-fun eps () EXT_FLT ((_ to_fp 11 56) RNE 0.00001))

; Variables and constants
(declare-fun x () FLT)
(declare-fun y () FLT)

(define-fun xe () EXT_FLT ((_ to_fp 11 56) RNE x))
(define-fun ye () EXT_FLT ((_ to_fp 11 56) RNE y))

(define-fun one () FLT ((_ to_fp 11 53) RNE 1.0))
(define-fun eone () EXT_FLT ((_ to_fp 11 56) RNE 1.0))

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

(define-fun re () EXT_FLT ((_ to_fp 11 56) RNE r))

; Extended functions
(define-fun t-ext () EXT_FLT (fp.mul RNE xe ye))
(define-fun r-ext () EXT_FLT
	    (fp.div RNE
	    	    (fp.sub RNE t-ext eone)
		    (fp.sub RNE (fp.mul RNE t-ext t-ext) eone)))

; Error bound
(assert (fp.gt (fp.abs (fp.sub RNE r-ext re)) eps))


(check-sat)
(get-model)
(exit)

