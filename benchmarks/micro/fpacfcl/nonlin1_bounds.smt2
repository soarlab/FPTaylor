; Variables and constants
(declare-fun t () (_ Float 11 52))

(define-fun one () (_ Float 11 52) ((_ fpnum 11 52) 4607182418800017408))
(define-fun zero () (_ Float 11 52) ((_ fpnum 11 52) 0))
(define-fun xxx () (_ Float 11 52) ((_ fpnum 11 52) 4607182418800017408))

(define-fun a () (_ Float 11 52) ((_ fpnum 11 52) 0))
(define-fun b () (_ Float 11 52) ((_ fpnum 11 52) 4651998512748167168))

; Bounds
(assert (and 
	(fple a t) (fple t b)))

; Functions
(define-fun r () (_ Float 11 52)
	    (fpdiv fp_round_nearest_even
	    	    t
		    (fpadd fp_round_nearest_even t one)))

; Bounds of the function
;(assert (or
;	(fplt r zero)
;	(fplt one r)))

(define-fun b1 () Bool (fplt r zero))
(define-fun b2 () Bool (fplt one r))
(define-fun bb () Bool (not (and (not b1) (not b2))))

(assert bb)

(check-sat)
;(get-model)
;(exit)

