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
(declare-const x0 FLT)
(declare-const x1 FLT)
(declare-const x2 FLT)

(define-fun x0e () EXT_FLT (ext x0))
(define-fun x1e () EXT_FLT (ext x1))
(define-fun x2e () EXT_FLT (ext x2))

; Bounds
(define-fun low () FLT (to-flt 1.0))
(define-fun high () FLT (to-flt 2.0))

(assert (and
		(<= low x0) (<= x0 high)
		(<= low x1) (<= x1 high)
		(<= low x2) (<= x2 high)))

; Function
(define-fun p0 () FLT (- RNE (+ RNE x0 x1) x2))
(define-fun p1 () FLT (- RNE (+ RNE x1 x2) x0))
(define-fun p2 () FLT (- RNE (+ RNE x2 x0) x1))
(define-fun sum () FLT (+ RNE (+ RNE p0 p1) p2))

(define-fun sume () EXT_FLT (ext sum))

; Higher precision function
(define-fun p0-ext () EXT_FLT (- RNE (+ RNE x0e x1e) x2e))
(define-fun p1-ext () EXT_FLT (- RNE (+ RNE x1e x2e) x0e))
(define-fun p2-ext () EXT_FLT (- RNE (+ RNE x2e x0e) x1e))
(define-fun sum-ext () EXT_FLT (+ RNE (+ RNE p0-ext p1-ext) p2-ext))

; Error bound
(assert (> (fp.abs (- RNE sum-ext sume)) eps))

(check-sat)
(get-model)
