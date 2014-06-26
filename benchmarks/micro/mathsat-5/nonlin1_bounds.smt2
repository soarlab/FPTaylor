(set-option :produce-models true)

(define-sort FLT () (_ FloatingPoint 11 53))

; Variables and constants
(declare-fun t () FLT)

(define-fun one () FLT ((_ to_fp 11 53) RNE 1.0))

; Bounds
(define-fun a () FLT ((_ to_fp 11 53) RNE 0.0))
(define-fun b () FLT ((_ to_fp 11 53) RNE 999.0))

(assert (and 
	(fp.leq a t) (fp.leq t b)))

; Functions
(define-fun r () FLT
	    (fp.div RNE
	    	    t
		    (fp.add RNE t one)))

; Bounds of the function
(assert (or
	(fp.lt r ((_ to_fp 11 53) RNE 0.0))
	(fp.gt r ((_ to_fp 11 53) RNE 1.0))))

(check-sat)
(get-model)
(exit)

