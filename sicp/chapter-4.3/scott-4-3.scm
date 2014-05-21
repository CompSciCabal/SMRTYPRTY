(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))
  
 
; p.50
; Deterministic primality test
; Time complexity: T(vn)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (define (next td)
; (if (= td 2) 3 (+ td 2))) ; Exercise 1.23, p.54
    (+ td 1))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (if (> n 1)
       (= (smallest-divisor n) n)
	   (false)))
	   
(define (an-element-of items)
	(require (not (null? items)))
	(amb (car items)
		(an-element-of (cdr items))))
		
(define (prime-sum-pair list1 list2)
	(let ((a (an-element-of list1))
		  (b (an-element-of list2)))
		(require (prime? (+ a b)))
		(list a b)))
	
(define L1 (list 1 3 5 8))
(define L2 (list 20 35 110))

(prime-sum-pair L1 L2)
try-again
try-again
try-again
	
(define (an-integer-starting-from n)
	(amb n (an-integer-starting-from (+ n 1))))
	
;; Exercise 4.35
(define (an-integer-between a b)
	(require (not (> a b)))
	(amb a (an-integer-between (+ a 1) b)))
	
(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high)))
		(let ((j (an-integer-between i high)))
			(let ((k (an-integer-between j high)))
				(require (= (+ (* i i) (* j j)) (* k k)))
				(list i j k)))))

;; Exercise 4.36
;; I think if you replaced between with starting from, it would only try to increment k and not i or j
;; Yes definitely after reviewing the evaluator, it will return to the last choice point if a require fails,
;; in this case the k integer
;; An approach (not efficient) is to make the all pythagorean triple generator have an outer let that
;; steadily increments high, while low is always set to 1.  This will repeat, a lot!  But it will 
;; in principle eventually generate them all.
(define (a-pythagorean-triple)
	(let ((high (an-integer-starting-from 1)))
		(a-pythagorean-triple-between 1 high)))
;; How to do this better?  Probably something like the stream merging stuff from the stream section.
;; will come back to that if I have time.

;; Exercise 4.37 
;; Is this more efficient?
;; So require that h^2 >= i^2 + j^2, why?  
;; low = 1, high = 5
;; i = 1, hsq = 25
;; j = 1, ksq = 2
;; k = sqrt(2) not int
;; j = 2, ksq = 3 not int
;; j = 3, ksq = 10 not int
;; Ok I see, the space explored is O(n^2) instead of O(n^3) where n = (high-low)  
;; Ben is as usual most correct.
(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high))
		  (hsq (* high high)))
		(let ((j (an-integer-between i high)))
			(let ((ksq (+ (* i i) (* j j))))
				(require (>= hsq ksq))
				(let ((k (sqrt ksq)))
					(require (integer? k))
					(list i j k))))))

;; Section 4.3.2
(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))
		  
(define (multiple-dwelling)
	(let ((baker (amb 1 2 3 4 5))
		  (cooper (amb 1 2 3 4 5))
		  (fletcher (amb 1 2 3 4 5))
		  (miller (amb 1 2 3 4 5))
		  (smith (amb 1 2 3 4 5)))
		(require(distinct? (list baker cooper fletcher miller smith)))
		(require (not (= baker 5)))
		(require (not (= cooper 1)))
		(require (not (= fletcher 5)))
		(require (not (= fletcher 1)))
		(require (> miller cooper))
		(require (not (= (abs (- smith fletcher)) 1)))
		(require (not (= (abs (- fletcher cooper)) 1)))
		(list (list "baker" baker)
			  (list "cooper" cooper)
			  (list "fletcher" fletcher)
			  (list "miller" miller)
			  (list "smith" smith))))
;; Exercise 4.38
;; Modify to remove the Smith-FLetcher connection, how many solutions?
(define (multiple-dwelling-mod)
	(let ((baker (amb 1 2 3 4 5))
		  (cooper (amb 1 2 3 4 5))
		  (fletcher (amb 1 2 3 4 5))
		  (miller (amb 1 2 3 4 5))
		  (smith (amb 1 2 3 4 5)))
		(require(distinct? (list baker cooper fletcher miller smith)))
		(require (not (= baker 5)))
		(require (not (= cooper 1)))
		(require (not (= fletcher 5)))
		(require (not (= fletcher 1)))
		(require (> miller cooper))
		(require (not (= (abs (- fletcher cooper)) 1)))
		(list (list "baker" baker)
			  (list "cooper" cooper)
			  (list "fletcher" fletcher)
			  (list "miller" miller)
			  (list "smith" smith))))
;; Running it seems to give 5 possible solutions.

;; Exercise 4.39
;; Does the order of the restrictions matter in multiple-dwelling?
;;  - Since we do a complete DFS, it can't affect the outcome, we will find it if it is there.
;;  - However, since we are doing a DFS if we find it earlier in our search then it will be faster
;;    so the order of restrictions could affect this, since each is a choice point and we drop out
;;    as soon as we find the first solution, we don't keep looking after that.
;; I don't have anything setup to time these, but if we set them up to minimize the amount of backtracking
;; the interpreter will do, then it should be faster.
;; They should be ordered from most to least restrictive, I'm not sure my selections are any better actually.
;; However if there was an ordering, such that the DFS arrives there first, then it could be significantly faster.
(define (multiple-dwelling-reordered)
	(let ((baker (amb 1 2 3 4 5))
		  (cooper (amb 1 2 3 4 5))
		  (fletcher (amb 1 2 3 4 5))
		  (miller (amb 1 2 3 4 5))
		  (smith (amb 1 2 3 4 5)))
		(require (not (= cooper 1)))
		(require (not (= fletcher 1)))
		(require (not (= (abs (- fletcher cooper)) 1)))
		(require (not (= fletcher 5)))
		(require (> miller cooper))
		(require (not (= baker 5)))
		(require(distinct? (list baker cooper fletcher miller smith)))
		(require (not (= (abs (- smith fletcher)) 1)))
		(list (list "baker" baker)
			  (list "cooper" cooper)
			  (list "fletcher" fletcher)
			  (list "miller" miller)
			  (list "smith" smith))))
;; Exercise 4.40
;; If floors aren't distinct then there are 5^5 possible floor assignments (5 people, 5 floors)
;; If assignments are unique then it drops to only 5!
;; Knowing the solution helps with selecting the ordering...
(define (multiple-dwelling-fast)
	(let ((smith (amb 1 2 3 4 5)))
		(let ((cooper (amb 1 2 3 4 5)))
			(require (not (= cooper 1)))
			(let ((fletcher (amb 1 2 3 4 5)))
				(require (not (= fletcher 1)))
				(require (not (= (abs (- smith fletcher)) 1)))
				(require (not (= fletcher 5)))
				(require (not (= (abs (- fletcher cooper)) 1)))
				(let ((baker (amb 1 2 3 4 5)))
					(require (not (= baker 5)))
					(let ((miller (amb 1 2 3 4 5)))
						(require (> miller cooper))
						(require(distinct? (list baker cooper fletcher miller smith)))
						(list (list "baker" baker)
							  (list "cooper" cooper)
							  (list "fletcher" fletcher)
							  (list "miller" miller)
							  (list "smith" smith))))))))

;; Exercise 4.41
;; write an ordinary scheme program for the multiple dwelling problem.
;; This is a pile...
;; scheme equivalent of the nested for loop.
;; I think there is likely a much nicer way to do this in ordinary scheme, but it was late.
(define (multiple-dwelling-ordinary)
	(define (isvalid? baker cooper fletcher miller smith)
		(cond ((= cooper 1) false)
		      ((= fletcher 1) false)
			  ((= (abs (- smith fletcher)) 1) false)
			  ((= fletcher 5) false)
			  ((= (abs (- fletcher cooper)) 1) false)
			  ((= baker 5) false)
			  ((not (> miller cooper)) false)
			  ((not (distinct? (list baker cooper fletcher miller smith))) false)
			  (else true))
	)
	(define (iter-s baker cooper fletcher miller smith) 
		(if (isvalid? baker cooper fletcher miller smith)
			(list (list "baker" baker)
				  (list "cooper" cooper)
				  (list "fletcher" fletcher)
				  (list "miller" miller)
				  (list "smith" smith))
			(if (= smith 5)
				(list)
				(iter-s baker cooper fletcher miller (+ smith 1))
			)
		)
	)
	(define (iter-m baker cooper fletcher miller)
		(let ((L (iter-s baker cooper fletcher miller 1)))
			(if (null? L)
				(if (= miller 5)
					L
					(iter-m baker cooper fletcher (+ miller 1)))
				L)
		)
	)
	(define (iter-f baker cooper fletcher)
		(let ((L (iter-m baker cooper fletcher 1)))
			(if (null? L)
				(if (= fletcher 5)
					L
					(iter-f baker cooper (+ fletcher 1)))
				L)
		)
	)
	(define (iter-c baker cooper)
		(let ((L (iter-f baker cooper 1)))
			(if (null? L)
				(if (= cooper 5)
					L
					(iter-c baker (+ cooper 1) ))
				L)
		)
	)
	(define (iter-b baker)
		(let ((L (iter-c baker 1)))
			(if (null? L)
				(if (= baker 5)
					L
					(iter-b (+ baker 1)))
				L)
		)
	)
	(iter-b 1)
)

;; Exercise 4.42
;; Liars Puzzle
;; So now we need to somehow require one thing and not require another, then try the opposite
;; Essentially given two statement A and B
;; we know either A & !B  is true or !A & B is true but neither may both be true or both be false
