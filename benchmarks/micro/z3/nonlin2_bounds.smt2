(set-logic QF_FPA)

(define-sort FLT () (_ FP 11 53))
(define-fun to-flt ((x Real)) FLT ((_ asFloat 11 53) RNE x))
;(define-sort FLT () (_ FP 8 24))
;(define-fun to-flt ((x Real)) FLT ((_ asFloat 8 24) RNE x))


(declare-const x FLT)
(declare-const y FLT)

(define-fun one () FLT (to-flt 1.0))
(define-fun low () FLT (to-flt 1.001))
(define-fun high () FLT (to-flt 2.0))

(assert (and (<= low x) (<= x high)
	     (<= low y) (<= y high)))

(define-fun t () FLT (* RNE x y))
(define-fun r () FLT 
	    (/ RNE
	     	(- RNE t one)
		(- RNE (* RNE t t) one)))

;(assert (< r (to-flt 0.2)))
(assert (or
	(< r (to-flt 0.2))
	(> r (to-flt 0.5))))


(check-sat)
(get-model)
