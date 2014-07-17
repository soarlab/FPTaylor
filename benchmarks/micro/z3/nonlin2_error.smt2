(set-logic QF_FPA)

(define-sort FLT () (_ FP 11 53))
(define-sort EXT_FLT () (_ FP 11 56))
(define-fun to-flt ((x Real)) FLT ((_ asFloat 11 53) RNE x))
(define-fun to-ext-flt ((x Real)) EXT_FLT ((_ asFloat 11 56) RNE x))
(define-fun ext ((x FLT)) EXT_FLT ((_ to_fp 11 56) RNE x))

;(define-sort FLT () (_ FP 8 24))
;(define-sort EXT_FLT () (_ FP 8 28))
;(define-fun to-flt ((x Real)) FLT ((_ asFloat 8 24) RNE x))
;(define-fun to-ext-flt ((x Real)) EXT_FLT ((_ asFloat 8 28) RNE x))
;(define-fun ext ((x FLT)) EXT_FLT ((_ to_fp 8 28) RNE x))

; epsilon
(define-fun eps () EXT_FLT (to-ext-flt 0.00001))

; Variables and constants
(declare-const x FLT)
(declare-const y FLT)

(define-fun xe () EXT_FLT (ext x))
(define-fun ye () EXT_FLT (ext y))

(define-fun one () FLT (to-flt 1.0))

; Bounds
(define-fun low () FLT (to-flt 1.001))
(define-fun high () FLT (to-flt 2.0))

(assert (and 
	(<= low x) (<= x high)
	(<= low y) (<= y high)))

; Function
(define-fun t () FLT (* RNE x y))
(define-fun r () FLT 
	    (/ RNE
	     	(- RNE t one)
		(- RNE (* RNE t t) one)))

(define-fun re () EXT_FLT (ext r))

; Higher precision function
(define-fun t-ext () EXT_FLT (* RNE xe ye))
(define-fun r-ext () EXT_FLT 
	    (/ RNE
	       (- RNE t-ext (ext one))
	       (- RNE (* RNE t-ext t-ext) (ext one))))

; Error bound
(assert (> (fp.abs (- RNE r-ext re)) eps))

(check-sat)
(get-model)
