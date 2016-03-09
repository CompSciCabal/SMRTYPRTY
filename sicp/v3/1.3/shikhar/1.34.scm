(define (f g)
  (g 2))

(f f)

; csi reports `Error: call of non-procedure: 2`

; this is because the body after substituting g for f is (f 2), which after further substitution is (2 2), which is an invalid procedure call