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
(define-fun eps () EXT_FLT (to-ext-flt 0.000016))

; Variables and constants
(declare-const t FLT)

(define-fun te () EXT_FLT (ext t))

(define-fun one () FLT (to-flt 1.0))

; Bounds
(define-fun low () FLT (to-flt 0.0))
(define-fun high () FLT (to-flt 999.0))

(assert (and (<= low t) (<= t high)))

; Function
(define-fun r () FLT
	    (/ RNE
	       t
	       (+ RNE t one)))

(define-fun re () EXT_FLT (ext r))

; Higher precision function
(define-fun r-ext () EXT_FLT 
	    (/ RNE
	       te
	       (+ RNE te (ext one))))

; Error bound
(assert (> (fp.abs (- RNE r-ext re)) eps))

(check-sat)
(get-model)
