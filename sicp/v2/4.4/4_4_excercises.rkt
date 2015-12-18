#|
Exercise 4.55
=============

Give simple queries that retrieve the following information from the data base:

    1) all people supervised by Ben Bitdiddle;
    A) (supervisor ? (Bitdiddle Ben))

    2) the names and jobs of all people in the accounting division;
    A) (job ?who (accounting . ?position))

    3) the names and addresses of all people who live in Slumerville.
    A) (address ?who (Slumerville . ?where))

|#

#|
Exercise 4.56
=============

Formulate compound queries that retrieve the following information:

1) the names of all people who are supervised by Ben Bitdiddle, together with their addresses;
A)(and
   (supervisor ?employee (Bitdiddle Ben))
   (address ?employee . ?address))


2) all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary;
A) (and
    (salary (Bitdiddle Ben) ?ben)
    (salary ?person ?amount)
    (lisp-value < ?amount ?ben))


3) all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job. 
A) (and
    (supervisor ?under ?over)
    (not (job ?over (computer . ?position))))
|#

#|
Exercise 4.57
=============

Define a rule that says that person 1 can replace person 2 if either person 1
does the same job as person 2 or someone who does person 1’s job can also do
person 2’s job, and if person 1 and person 2 are not the same person. Using your
rule, give queries that find the following:

(assert!
 (rule (can-replace ?replacing ?replaced)
       (and
        (or
         ; person one has the same job description
         (and (job ?replaced . ?job) 
              (job ?replacing . ?job))
         (and
          ; person one can do person two's job
          (job ?replaced ?replaced-job)
          (job ?replacing ?replacing-job)
          (can-do-job ?replacing-job ?replaced-job)))
        (not (same ?replacing ?replaced)))))

1) all people who can replace Cy D. Fect

(can-replace (fect cy d) (reasoner louis))
(can-replace (fect cy d) (hacker alyssa p))

Note: Louise Reasoner is a bug, they are a 'computer programmer trainee', so shouldn't really be considered a match. 

2) all people who can replace someone who is being paid more than they are, together with the two salaries.

(and (can-replace ?replacing ?replaced)
     (salary ?replacing ?lower-salary)
     (salary ?replaced ?higher-salary)
     (lisp-value > ?higher-salary ?lower-salary))
Result:
(and (can-replace (aull dewitt) (warbucks oliver)) (salary (aull dewitt) 25000) (salary (warbucks oliver) 150000) (lisp-value > 150000 25000))
(and (can-replace (fect cy d) (hacker alyssa p)) (salary (fect cy d) 35000) (salary (hacker alyssa p) 40000) (lisp-value > 40000 35000))
|#

#|
Exercise 4.58
=============

Define a rule that says that a person is a “big shot” in a division if the person works
in the division but does not have a supervisor who works in the division.

Initial busted attempts at solving this looked like this:

(assert!
(rule (big-shot ?person)
      (and
       (job ?person (?division . ?description))      
       (outranked-by ?person ?boss)
       (not (job ?boss (?division . ?x))))))

I assumed, apparently incorrectly, that it was okay to infer which division we're talking about
based on the name of the person, because in this db, no one works in more than one division, and
there are many other unrealistic aspects to this toy situation.

Eventually I got tired of fighting with this approach and looked online, found two answers, both
of which explicitly take the division as an arg:

(assert! (rule (bigshot ?person ?division) 
                (and (job ?person (?division . ?rest)) 
                     (or (not (supervisor ?person ?boss)) 
                         (and (supervisor ?person ?boss) 
                              (not (job ?boss (?division . ?r))))))))
|#

#|
Exercise 4.59
=============

Ben Bitdiddle has missed one meeting too many. Fearing that his habit of forgetting
meetings could cost him his job, Ben decides to do something about it. He adds all
the weekly meetings of the firm to the Microshaft data base by asserting the following:

(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

Each of the above assertions is for a meeting of an entire division. Ben also adds an
entry for the company-wide meeting that spans all the divisions. All of the company’s
employees attend this meeting.

(meeting whole-company (Wednesday 4pm))

1) On Friday morning, Ben wants to query the data base for all the meetings that occur that day.
   What query should he use?

(meeting ?x (friday ?y))

2) Alyssa P. Hacker is unimpressed. She thinks it would be much more useful to be able to
   ask for her meetings by specifying her name. So she designs a rule that says that a person’s
   meetings include all whole-company meetings plus all meetings of that person’s division. Fill
   in the body of Alyssa’s rule.

(rule
 (meeting-time ?person ?day-and-time)
 (or
  (meeting whole-company ?day-and-time)
  (and
   (job ?person (?division . ?job))
   (meeting ?division ?day-and-time)
   )))

3) Alyssa arrives at work on Wednesday morning and wonders what meetings she has to attend that
   day. Having defined the above rule, what query should she make to find this out?

(meeting-time (hacker alyssa p) (wednesday ?when))
|#

#|
Exercise 4.60
=============

By giving the query

(lives-near ?person (Hacker Alyssa P))

Alyssa P. Hacker is able to find people who live near her, with whom she can ride to work.
On the other hand, when she tries to find all pairs of people who live near each other by querying

(lives-near ?person-1 ?person-2)

she notices that each pair of people who live near each other is listed twice; for example,

(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

Why does this happen? Is there a way to find a list of people who live near each other, in which
each pair appears only once? Explain. 

Answer
------

Because the rule has no notion of commutivity. If A is near B, B is near A, however the rule doesn't
have any way of filtering the redundant relationship.
|#
(assert!
(rule (lives-near1 ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))
           (not (lives-near1 ?person-2 ?person-1)))))

#|
Exercise 4.62
=============

Define rules to implement the last-pair operation of Exercise 2.17, which returns a list containing
the last element of a nonempty list. Check your rules on queries such as (last-pair (3) ?x),
(last-pair (1 2 3) ?x) and (last-pair (2 ?x) (3)). Do your rules work correctly on queries such as
(last-pair ?x (3))?

(last-pair (3) ?x) -> (3)
(last-pair (1 2 3) ?x) -> (3)
(last-pair (2 ?x) (3)) -> (3)

|#

(assert!
 (rule (last-pair (?x '()) (?x))))
(assert!
 (rule (last-pair (?v . ?x) (?x))
       (last-pair (?x) (?x))))