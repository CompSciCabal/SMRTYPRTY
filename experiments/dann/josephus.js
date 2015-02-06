// A few small JS functions for experimenting with the Josephus problem. Try it in your browser console!


j = function(n, q) {
  q = q || 2
  var list = range(n)
  var p = q < 0 ? 1 : -1
  var seq = []
  while(list.length > 1) {
    p = (p+q) % list.length
    if(p<0) p = list.length + p
    seq.push(list[p])
    list.splice(p, 1) // OPT
    if(q >= 0) p--
  }

  return seq.concat(list[0]||0)
}

range = function(len, off) {
  off = off|0
  var list = []
  for(var i=1; i<=len; i++) 
    list.push(i+off)

  return list
}

last = function(list, k) { return list.slice(-k) }

survive = function(n, q, k) {return last(j(n, q), k||(q-1)||1)}

// x = r.map(function(n) { return r.map(function(q) { return survive(n, q, 1)[0] }) })
// console.table(x)

// q = [0,1]; for(var i=2; i<1000; i++) { q.push(survive(i,i,1)[0] )}
// q.filter(function(s, i) {return s+1 == i}).map(function(s) {return s+1})
// q.filter(function(s, i) {return s+2 == i}).map(function(s) {return s+2})
// q.filter(function(s, i) {return s*2 == i}).map(function(s) {return s*2})
