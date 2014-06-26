(set-logic QF_FPA)

(define-sort FLT () (_ FP 11 53))
(define-fun to-flt ((x Real)) FLT ((_ asFloat 11 53) RNE x))
;(define-sort FLT () (_ FP 8 24))
;(define-fun to-flt ((x Real)) FLT ((_ asFloat 8 24) RNE x))


(declare-const t FLT)

(define-fun one () FLT (to-flt 1.0))
(define-fun low () FLT (to-flt 0.0))
(define-fun high () FLT (to-flt 999.0))

(assert (and (<= low t) (<= t high)))

(define-fun r () FLT 
	    (/ RNE
	        t
		(+ RNE t one)))

(assert (or
	(< r (to-flt 0.0))
	(> r (to-flt 1.0))))


(check-sat)
(get-model)
