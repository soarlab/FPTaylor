(set-option :produce-models true)

(define-sort FLT () (_ FloatingPoint 11 53))
(define-sort EXT_FLT () (_ FloatingPoint 11 56))

; Epsilon
(define-fun eps () EXT_FLT ((_ to_fp 11 56) RNE 0.00001))

; Variables and constants
(declare-fun t () FLT)
(define-fun te () EXT_FLT ((_ to_fp 11 56) RNE t))

(define-fun one () FLT ((_ to_fp 11 53) RNE 1.0))
(define-fun eone () EXT_FLT ((_ to_fp 11 56) RNE 1.0))

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

(define-fun re () EXT_FLT ((_ to_fp 11 56) RNE r))

; Extended functions
(define-fun r-ext () EXT_FLT
	    (fp.div RNE
	    	    te
		    (fp.add RNE te eone)))

; Error bound
(assert (fp.gt (fp.abs (fp.sub RNE r-ext re)) eps))


(check-sat)
(get-model)
(exit)

