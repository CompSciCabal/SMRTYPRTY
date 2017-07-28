#lang scheme

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length br)
  (car br))
(define (branch-structure br)
  (car (cdr br)))

(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

(define (total-weight-branch branch)
  (define branch-struct (branch-structure branch))
  (cond [(number? branch-struct) branch-struct]
        [else (total-weight branch-struct)]))

(define (balanced? mobile)
  ;; returns #f or the weight of the mobile
  (define left-weight (balanced-branch? (left-branch mobile))) ;; could be #f
  (define right-weight (balanced-branch? (right-branch mobile)))
  (displayln left-weight)
  (displayln right-weight)
  (cond [(or (boolean? left-weight) (boolean? right-weight)) #f]
        [(= (* (branch-length (left-branch mobile)) left-weight)
            (* (branch-length (right-branch mobile)) right-weight))
         (+ left-weight right-weight)]
        [else #f]))

(define (balanced-branch? branch)
  ;; returns #f or the weight of this branch
  (define branch-struct (branch-structure branch))
  (cond [(number? branch-struct) branch-struct]
        [else (balanced? branch-struct)]))

(define simple-mobile (make-mobile
                       (make-branch 4 2)
                       (make-branch 1 4)))

(define my-mobile (make-mobile
                   (make-branch 5 (make-mobile
                                   (make-branch 3 8)
                                   (make-branch 2 9)))
                   (make-branch 1 (make-mobile
                                   (make-branch 5 (make-mobile
                                                   (make-branch 2 4)
                                                   (make-branch 4 2)))
                                   (make-branch 4 2)))))
(balanced? simple-mobile)