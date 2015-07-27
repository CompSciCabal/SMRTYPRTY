#lang racket
#|
Concurrency! Now we're talking! I actually found the constraint solving section
kind of a let down -- preferred it to the electronics sim section, and the techniques/
code is cool, but felt like a lot of heat for not so much light -- would have been 
nice to go deeper into that stuff. 

The concurrency section so far has been super interesting, though.

Exercise 3.38
-------------
  Suppose that Peter, Paul, and Mary share a joint bank account that initially 
contains $100. Concurrently, Peter deposits $10, Paul withdraws $20, and Mary 
withdraws half the money in the account, by executing the following commands:
Peter:	(set! balance (+ balance 10))
Paul:	(set! balance (- balance 20))
Mary:	(set! balance (- balance (/ balance 2)))

a. List all the different possible values for balance after these three transactions 
have been completed, assuming that the banking system forces the three processes to 
run sequentially in some order.
 
A:
Pe, Pa, M: (110 - 20)/ 2 = 45
Pe, M, Pa: 110/2 - 20 = 35
M, Pe, Pa: 100/2 + 10 - 20 = 40
M, Pa, Pe: 100/2 - 20 + 10 = 40
Pa, Pe, M: (80 + 10)/2 = 45
Pa, M, Pe: 80/2 + 10 = 50

b. What are some other values that could be produced if the system allows the processes 
to be interleaved? Draw timing diagrams like the one in figure 3.29 to explain how these 
values can occur.
|#
