#lang pyret/check

fun square(n):
  n * n
where:
  square(2) is 4
  square(4) is 16
  square(8) is 64
end