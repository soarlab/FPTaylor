(set-option :produce-models true)

(define-sort FLT () (_ FloatingPoint 11 53))
(define-sort EXT_FLT () (_ FloatingPoint 11 56))

; Epsilon
(define-fun eps () EXT_FLT ((_ to_fp 11 56) RNE 0.00001))

; Variables and constants
(declare-fun x0 () FLT)
(declare-fun x1 () FLT)
(declare-fun x2 () FLT)

(define-fun x0e () EXT_FLT ((_ to_fp 11 56) RNE x0))
(define-fun x1e () EXT_FLT ((_ to_fp 11 56) RNE x1))
(define-fun x2e () EXT_FLT ((_ to_fp 11 56) RNE x2))

; Bounds
(define-fun a () FLT ((_ to_fp 11 53) RNE 1.0))
(define-fun b () FLT ((_ to_fp 11 53) RNE 2.0))

(assert (and 
	(fp.leq a x0) (fp.leq x0 b)
	(fp.leq a x1) (fp.leq x1 b)
	(fp.leq a x2) (fp.leq x2 b)))

; Functions
(define-fun p0 () FLT (fp.sub RNE (fp.add RNE x0 x1) x2))
(define-fun p1 () FLT (fp.sub RNE (fp.add RNE x1 x2) x0))
(define-fun p2 () FLT (fp.sub RNE (fp.add RNE x2 x0) x1))
(define-fun sum () FLT (fp.add RNE (fp.add RNE p0 p1) p2))

(define-fun sume () EXT_FLT ((_ to_fp 11 56) RNE sum))

; Extended functions
(define-fun p0-ext () EXT_FLT (fp.sub RNE (fp.add RNE x0e x1e) x2e))
(define-fun p1-ext () EXT_FLT (fp.sub RNE (fp.add RNE x1e x2e) x0e))
(define-fun p2-ext () EXT_FLT (fp.sub RNE (fp.add RNE x2e x0e) x1e))
(define-fun sum-ext () EXT_FLT (fp.add RNE (fp.add RNE p0-ext p1-ext) p2-ext))


; Error bound
(assert (fp.gt (fp.abs (fp.sub RNE sum-ext sume)) eps))


(check-sat)
(get-model)
(exit)

