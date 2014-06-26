(set-logic QF_FPA)

(define-sort FLT () (_ FP 11 53))
(define-fun to-flt ((x Real)) FLT ((_ asFloat 11 53) RNE x))
;(define-sort FLT () (_ FP 8 24))
;(define-fun to-flt ((x Real)) FLT ((_ asFloat 8 24) RNE x))

; Variables
(declare-const x0 FLT)
(declare-const x1 FLT)
(declare-const x2 FLT)

; Subexpressions
(define-fun p0 () FLT (- RNE (+ RNE x0 x1) x2))
(define-fun p1 () FLT (- RNE (+ RNE x1 x2) x0))
(define-fun p2 () FLT (- RNE (+ RNE x2 x0) x1))
(define-fun sum () FLT (+ RNE (+ RNE p0 p1) p2))

; Bounds of variables
(define-fun low () FLT (to-flt 1.0))
(define-fun high () FLT (to-flt 2.0))

(assert (and
		(<= low x0) (<= x0 high)
		(<= low x1) (<= x1 high)
		(<= low x2) (<= x2 high)))

; Bounds of the function
(assert (or
	(< sum (to-flt 3.0))
	(> sum (to-flt 6.0))))


(check-sat)
(get-model)
