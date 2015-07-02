#lang racket

(displayln "exercise 3.38")
;; Maybe Later

(displayln "exercise 3.39")
#|
| The values of x that are possible are:
| - 101
| - 121
| - 100
|   - This is because the first item gets
|     reserialized so it could read the value
|     set it, but the other function could be
|     executing in between. Data will get lost
|#

(displayln "exercise 3.40")
;; a. Maybe later
;; But basically there can be state change in
;; between the various reads of x

#|
| b. 
| 1000000: x = 10^2; x = 100^3
| 1000000: x = 10^3; x = 1000^2
|#

(displayln "exercise 3.41")
;; The improvement Ben makes to the account is only
;; for reads, which is only valid at the point that
;; it is read. After we have made that read something
;; could have adjusted it. So the gains we get from
;; having the read serialized are basically neutralized

(displayln "exercise 3.42")
;; They are functionally equivalent, Bens just results
;; in fewer allocations.