(define (cube x) (* x x x))
(define (p x)
  (print x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; a)
      ; > (sine 12.15)
      ; 0.05
      ; 0.1495
      ; 0.4351345505
      ; 0.975846533167877
      ; -0.789563114470823
      ; -0.39980345741334

;; b) log_3 n
