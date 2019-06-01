(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/numbers.scm")
(load "faster-miniKanren/test-check.scm")

;;; William E. Byrd

;;; Exercises from Peter Norvig's 'Paradigms of Artificial Intelligence Programming'

;;; All code released under MIT licence.

;; TODO
;;
;; * thread the tracing through the entire GPS monadically to capture all states and ops
;;
;; * add tests for real
;;
;; * add tests for inventing lists of ops, inventing the starting state, etc.
;;
;; * make more efficient running backwards
;;
;; * right the code can construct multisets instead of sets when running backwards--can we fix this?


(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        ((== x y))
        ((=/= x y)
         (membero x rest))))))

(define not-membero
  (lambda (x ls)
    (conde
      ((== '() ls))
      ((fresh (y rest)
         (== `(,y . ,rest) ls)
         (=/= x y)
         (not-membero x rest))))))

(define removeo
  (lambda (x ls ls-x)
    (conde
      ((== '() ls)
       (== ls ls-x))
      ((fresh (e rest)
         (== `(,e . ,rest) ls)
         (conde
           ((== x e)
            (removeo x rest ls-x))
           ((=/= x e)
            (fresh (ls^)
              (== `(,e . ,ls^) ls-x)
              (removeo x rest ls^)))))))))

(define set-differenceo
  (lambda (s1 s2 s1-s2)
    (conde
      ((== '() s2)
       (== s1 s1-s2))
      ((fresh (e rest s1^)
         (== `(,e . ,rest) s2)
         (removeo e s1 s1^)
         (set-differenceo s1^ rest s1-s2))))))

(define uniono
  (lambda (s1 s2 s1+s2)
    (conde
      ((== '() s1)
       (== s2 s1+s2))
      ((fresh (e rest)
         (== `(,e . ,rest) s1)
         (conde
           ((membero e s2)
            (uniono rest s2 s1+s2))
           ((fresh (s^)
              (== `(,e . ,s^) s1+s2)
              (not-membero e s2)
              (uniono rest s2 s^)))))))))




(define *school-ops*
  '((drive-son-to-school ;; action
     (son-at-home car-works) ;; preconds
     (son-at-school) ;; add-list
     (son-at-home) ;; del-list
     )
    (shop-installs-battery
     (car-needs-battery shop-knows-problem shop-has-money)
     (car-works)
     ())
    (tell-shop-problem
     (in-communication-with-shop)
     (shop-knows-problem)
     ())
    (telephone-shop
     (know-phone-number)
     (in-communication-with-shop)
     ())
    (look-up-number
     (have-phone-book)
     (know-phone-number)
     ())
    (give-shop-money
     (have-money)
     (shop-has-money)
     (have-money))))

(define gpso
  (lambda (state goals ops state^ trace)
    (let loop ((g* goals)
               (s state)
               (t `(,state)))
      (conde
        ((== '() g*)
         (== s state^)
         (== `(solved . ,t) trace))
        ((fresh (g rest s^)
           (== `(,g . ,rest) g*)
           (achieveo g ops s s^)
           (loop rest s^ `(,s^ . ,t))))))))

(define achieveo
  (lambda (goal ops state state^)
    (conde
      ((== state state^)
       (membero goal state))
      ((not-membero goal state)
       (let loop ((o* ops))
         (fresh (o rest)
           (== `(,o . ,rest) o*)
           (conde
             ((appropriateo goal o)
              (apply-opo o ops state state^))
             ((loop rest)))))))))

(define appropriateo
  (lambda (goal op)
    (fresh (action preconds add-list del-list)
      (== `(,action ,preconds ,add-list ,del-list) op)
      (membero goal add-list))))

(define apply-opo
  (lambda (op ops state state^)
    (fresh (action preconds add-list del-list)
      (== `(,action ,preconds ,add-list ,del-list) op)
      (let loop ((p* preconds)
                 (s state))
        (conde
          ((== '() p*)
           (fresh (s^)
             (set-differenceo s del-list s^)
             (uniono s^ add-list state^)))
          ((fresh (p rest s^)
             (== `(,p . ,rest) p*)
             (achieveo p ops s s^)
             (loop rest s^))))))))


(run 1 (ops)
  (apply-opo
   '(drive-son-to-school ;; action
     (son-at-home car-works) ;; preconds
     (son-at-school) ;; add-list
     (son-at-home) ;; del-list
     )
    ops
    '(son-at-home car-works) ;; state
    '(car-works son-at-school) ;; state^
    ))


#!eof

(run 1 (ops)
  (gpso
   '(son-at-home car-needs-battery have-money have-phone-book) ;; state
   '(son-at-school) ;; goals
   ops     ;; ops
   '(car-needs-battery have-phone-book know-phone-number
                       in-communication-with-shop shop-knows-problem shop-has-money
                       car-works son-at-school)               ;; state^
   '(solved
     (car-needs-battery have-phone-book know-phone-number
                        in-communication-with-shop shop-knows-problem shop-has-money
                        car-works son-at-school)
     (son-at-home
      car-needs-battery
      have-money
      have-phone-book)) ;; trace
   ))

#!eof

(run 1 (s^ t)
  (gpso
   '(son-at-home car-needs-battery have-money have-phone-book) ;; state
   '(son-at-school) ;; goals
   *school-ops*     ;; ops
   s^               ;; state^
   t                ;; trace
   ))

=>
(((car-needs-battery have-phone-book know-phone-number
                     in-communication-with-shop shop-knows-problem shop-has-money
                     car-works son-at-school)
  (solved
   (car-needs-battery have-phone-book know-phone-number
                      in-communication-with-shop shop-knows-problem shop-has-money
                      car-works son-at-school)
   (son-at-home
    car-needs-battery
    have-money
    have-phone-book))))

#!eof


(run 1 (q t)
  (gpso
   '(son-at-home car-needs-battery have-money have-phone-book) ;; state
   '(son-at-school) ;; goals
   *school-ops* ;; ops
   q ;; state^
   t ;; trace
   ))


#!eof

(run 1 (op state^)
  (apply-opo
    op
    *school-ops*
    '(son-at-home car-works) ;; state
    state^))

#!eof

(run 1 (state^)
  (apply-opo
   '(drive-son-to-school ;; action
     (son-at-home car-works) ;; preconds
     (son-at-school) ;; add-list
     (son-at-home) ;; del-list
     )
    *school-ops*
    '(son-at-home car-works) ;; state
    state^))


#!eof

(run 1 (q t)
  (gpso
   '(son-at-home car-works) ;; state
   '(son-at-school) ;; goals
   *school-ops* ;; ops
   q ;; state^
   t ;; trace
   ))


#!eof

(run 1 (q t)
  (gpso
   '(son-at-school) ;; state
   '(son-at-school) ;; goals
   *school-ops* ;; ops
   q ;; state^
   t ;; trace
   ))

#!eof

(run 1 (q t)
  (gpso
   '(son-at-home car-needs-battery have-money have-phone-book) ;; state
   '(son-at-school) ;; goals
   *school-ops* ;; ops
   q ;; state^
   t ;; trace
   ))
