;; -------------------------------------------------------
;; Microshaft Database (install into evaluator)
;; -------------------------------------------------------

(add-rule-or-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(add-rule-or-assertion! '(job (Bitdiddle Ben) (computer wizard)))
(add-rule-or-assertion! '(salary (Bitdiddle Ben) 60000))
(add-rule-or-assertion! '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(add-rule-or-assertion! '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-rule-or-assertion! '(job (Hacker Alyssa P) (computer programmer)))
(add-rule-or-assertion! '(salary (Hacker Alyssa P) 40000))
(add-rule-or-assertion! '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(add-rule-or-assertion! '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
(add-rule-or-assertion! '(job (Fect Cy D) (computer programmer)))
(add-rule-or-assertion! '(salary (Fect Cy D) 35000))
(add-rule-or-assertion! '(supervisor (Fect Cy D) (Bitdiddle Ben)))

(add-rule-or-assertion! '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-rule-or-assertion! '(job (Tweakit Lem E) (computer technician)))
(add-rule-or-assertion! '(salary (Tweakit Lem E) 25000))
(add-rule-or-assertion! '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(add-rule-or-assertion! '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-rule-or-assertion! '(job (Reasoner Louis) (computer programmer trainee)))
(add-rule-or-assertion! '(salary (Reasoner Louis) 30000))
(add-rule-or-assertion! '(supervisor (Reasoner Louis) (Hacker Alyssa P)))

(add-rule-or-assertion! '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-rule-or-assertion! '(job (Warbucks Oliver) (administration big wheel)))
(add-rule-or-assertion! '(salary (Warbucks Oliver) 150000))

(add-rule-or-assertion! '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
(add-rule-or-assertion! '(job (Scrooge Eben) (accounting chief accountant)))
(add-rule-or-assertion! '(salary (Scrooge Eben) 75000))
(add-rule-or-assertion! '(supervisor (Scrooge Eben) (Warbucks Oliver)))

(add-rule-or-assertion! '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-rule-or-assertion! '(job (Cratchet Robert) (accounting scrivener)))
(add-rule-or-assertion! '(salary (Cratchet Robert) 18000))
(add-rule-or-assertion! '(supervisor (Cratchet Robert) (Scrooge Eben)))

(add-rule-or-assertion! '(address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-rule-or-assertion! '(job (Aull DeWitt) (administration secretary)))
(add-rule-or-assertion! '(salary (Aull DeWitt) 25000))
(add-rule-or-assertion! '(supervisor (Aull DeWitt) (Warbucks Oliver)))

(add-rule-or-assertion! '(can-do-job (computer wizard) (computer programmer)))
(add-rule-or-assertion! '(can-do-job (computer wizard) (computer technician)))
(add-rule-or-assertion! '(can-do-job (computer programmer) (computer programmer trainee)))
(add-rule-or-assertion! '(can-do-job (administration secretary) (administration big wheel)))

(add-rule-or-assertion! '(rule (same (? x) (? x))))

;; -------------------------------------------------------
;; Rules and Queries (type into evaluator)
;; -------------------------------------------------------

(assert!
  (rule (lives-near ?person-1 ?person-2)
        (and (address ?person-1 (?town . ?rest-1))
             (address ?person-2 (?town . ?rest-2))
             (not (same ?person-1 ?person-2)))))

;; -------------------------------------------------------
;; Query Language
;; -------------------------------------------------------

;; Exercise 4.55, p.446

(supervisor ?x (Bitdiddle Ben))
(job ?x (accounting . ?y))
(address ?x (Slumerville . ?y))

;; Exercise 4.56, p.448

(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?address))

(and (salary (Bitdiddle Ben) ?bens-salary)
     (salary ?person ?salary)
     (lisp-value > ?bens-salary ?salary))

(and (job ?supervisor ?div)
     (not (job ?supervisor (computer . ?job)))
     (supervisor ?person ?supervisor))

;; Exercise 4.57, p.450

; Note: the order of queries matters!
(assert!
  (rule (can-replace ?person-1 ?person-2)
        (and (job ?person-1 ?job-1)
             (job ?person-2 ?job-2)
             (or (same ?job-1 ?job-2)
                 (can-do-job ?job-1 ?job-2))
             (not (same ?person-1 ?person-2)))))

(can-replace ?person (Fect Cy D))

;= (can-replace (Bitdiddle Ben) (Fect Cy D))
;= (can-replace (Hacker Alyssa P) (Fect Cy D))

(and (can-replace ?replacer ?replacee)
     (salary ?replacer ?small)
     (salary ?replacee ?big)
     (lisp-value > ?big ?small))

;= (and (can-replace (Aull DeWitt) (Warbucks Oliver))
;=      (salary (Aull DeWitt) 25000)
;=      (salary (Warbucks Oliver) 150000)
;=      (lisp-value > 150000 25000))
;= (and (can-replace (Fect Cy D) (Hacker Alyssa P))
;=      (salary (Fect Cy D) 35000)
;=      (salary (Hacker Alyssa P) 40000)
;=      (lisp-value > 40000 35000))

;; Exercise 4.58, p.450

(assert!
  (rule (big-shot ?person ?div)
        (and (job ?person (?div . ?x))
             (or (not (supervisor ?person ?s))
                 (and (supervisor ?person ?supervisor)
                      (not (job ?supervisor (?div . ?y))))))))

(big-shot ?person ?div)

;= (big-shot (Warbucks Oliver) administration)
;= (big-shot (Scrooge Eben) accounting)
;= (big-shot (Bitdiddle Ben) computer)

;; Exercise 4.59, p.450

(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

(meeting ?div (Friday ?time))

;= (meeting administration (Friday 1pm))

(assert!
  (rule (meeting-time ?person ?day-and-time)
        (or (meeting whole-company ?day-and-time)
            (and (job ?person (?div . ?x))
                 (meeting ?div ?day-and-time)))))

(and (meeting-time (Hacker Alyssa P) (Wednesday ?time))
     (meeting ?div (Wednesday ?time)))

;= (and (meeting-time (Hacker Alyssa P) (Wednesday 4pm))
;=      (meeting whole-company (Wednesday 4pm)))
;= (and (meeting-time (Hacker Alyssa P) (Wednesday 3pm))
;=      (meeting computer (Wednesday 3pm)))

;; Exercise 4.60, p.451

(lives-near ?person (Hacker Alyssa P))
(live-near ?person-1 ?person-2)

(assert!
  (rule (live-near ?person-1 ?person-2)
        (and (address ?person-1 (?town . ?rest-1))
             (address ?person-2 (?town . ?rest-2))
             (lisp-value slist<? ?person-1 ?person-2))))

;; -------------------------------------------------------
;; Logic as Programs
;; -------------------------------------------------------

(assert!
  (rule (append-to-form () ?y ?y)))
(assert!
  (rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z)))

(append-to-form (a b) (c d) ?z)
(append-to-form ?x ?y (a b c d))

;; Exercise 4.61, p.452

(assert!
  (rule (?x next-to ?y in (?x ?y . ?u))))
(assert!
  (rule (?x next-to ?y in (?v . ?z))
        (?x next-to ?y in ?z)))

(?x next-to ?y in (1 (2 3) 4))

;= ((2 3) next-to 4 in (1 (2 3) 4))
;= (1 next-to (2 3) in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))

;= (3 next-to 1 in (2 1 3 1))
;= (2 next-to 1 in (2 1 3 1))

;; Exercise 4.62, p.453

(assert!
  (rule (last-pair (?x) (?x))))
(assert!
  (rule (last-pair (?x . ?y) ?z)
        (last-pair ?y ?z)))

(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))

(last-pair ?x (3)) ;= stuck

;; Exercise 4.63, p.453

(add-rule-or-assertion! '(son Adam Cain))
(add-rule-or-assertion! '(son Cain Enoch))
(add-rule-or-assertion! '(son Enoch Irad))
(add-rule-or-assertion! '(son Irad Mehujael))
(add-rule-or-assertion! '(son Mehujael Methushael))
(add-rule-or-assertion! '(son Methushael Lamech))
(add-rule-or-assertion! '(wife Lamech Ada))
(add-rule-or-assertion! '(son Ada Jabal))
(add-rule-or-assertion! '(son Ada Jubal))

(assert!
  (rule (grandson ?g ?s)
        (and (son ?g ?f) (son ?f ?s))))

(assert!
  (rule (son ?f ?s)
        (and (wife ?f ?w) (son ?w ?s))))

(grandson Cain ?x)
(son Lamech ?x) ; wow!
(grandson Methushael ?x)

;; -------------------------------------------------------
;; Logic Programming and Mathematical Logic
;; -------------------------------------------------------

;; Infinite loops

(assert! (married Minnie Mickey))
(assert! (rule (married ?x ?y)
               (married ?y ?x)))

(married ?x Minnie)

;; not

(not (baseball-fan (Bitdiddle Ben)))
(not (raining outside))
(not (equal 4 (add 2 2)))

;; Exercise 4.64, p.466
;; Quering this rule works

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;; Quering this rule stuck in infinite loop
;; because ?middle-manager in line *** is unbound

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss) ;***
               (supervisor ?staff-person ?middle-manager))))

;; Exercise 4.65, p.467
;; Both vars in line *** are unbound

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person) ;***
           (supervisor ?x ?middle-manager)))

;; 2 middle managers, Scrooge Eben and Bitdiddle Ben,

(supervisor (Cratchet Robert) (Scrooge Eben))
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

;; reporting to Warbucks Oliver,

(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(supervisor (Scrooge Eben) (Warbucks Oliver))

;; have 4 subordinates

(supervisor (Cratchet Robert) (Scrooge Eben))
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))
(supervisor (Tweakit Lem E) (Bitdiddle Ben))
(supervisor (Fect Cy D) (Bitdiddle Ben))

;; Exercise 4.68, p.468
;; Install append-to-form rule

(assert! (rule (reverse () ())))
(assert! (rule (reverse (?u . ?v) ?z)
               (and (reverse ?v ?y)
                    (append-to-form ?y (?u) ?z))))

(reverse (1 2 3) ?x) ;= (reverse (1 2 3) (3 2 1))
(reverse ?x (1 2 3)) ; infinite loop

;; Exercise 4.69, p.468
;; Install grandson and son rules

(assert! (rule (ends-in-grandson (grandson))))
(assert! (rule (ends-in-grandson (?g . ?gs))
               (ends-in-grandson ?gs)))

(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))
(assert! (rule ((great . ?rel) ?x ?y)
               (and (son ?x ?z)
                    (?rel ?z ?y)
                    (ends-in-grandson ?rel))))

((great grandson) Adam Irad)
((great grandson) ?g ?ggs)
(?relationship Adam Irad)
