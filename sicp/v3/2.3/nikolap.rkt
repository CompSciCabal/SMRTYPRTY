;; 2.33
(define (map p sequence)
 (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
 (accumulate cons seq2 seq1))

(define (length sequence)
 (accumulate (lambda (x y) (+ y 1)) 0 sequence)

;; 2.34
(define (horner-eval x coefficient-sequence)
 (accumulate (lambda (this-coeff higher-terms)
              (+ this-coeff (* x higher-terms)))
             0
             coefficient-sequence))

;; 2.35
(define (count-leaves t)
 (accumulate + 0
   (map (lambda (_) 1) (enumerate-tree t))))

;; 2.36
(define (accumulate-n op init seqs)
 (if (null? (car seqs))
  nil
  (cons (accumulate op init (map cdr seqs))
        (accumulate-n op init (map cdr seqs)))))

;; 2.37
(define (dot-product v w)
 (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
 (map (labmda (x)
       (dot-product x v)) m))

(define (transpose mat)
 (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
 (let ((cols (transpose n))
       (map (lambda (x)
             (matrix-*-vector cols x)) m))))

;; 2.38

;; 2.39

;; 2.40

;; 2.41

;; 2.42

;; 2.43
