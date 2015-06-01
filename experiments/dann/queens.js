<!doctype html>
<html>
<head><title>Queens Puzzle</title></head>
<body>

  <script type="text/javascript">
    /*
        8 QUEENS PROBLEM IN JS

        The first version is a fairly direct translation of the scheme version to JS,
        where the flatmap is replaced by map->reduce(concat).
        It's pretty nifty that this is even possible in JS.

        It's slow, though, so we optimize away the massive number of flatmap allocations
        (scheme must do this automatically). Amazingly, this one optimization makes queens2
        faster than the scheme version, despite the fact that this style of JS is much less
        optimal than using nested for-loops.
    */
    queens = function(size) {
      var interval = Array.apply(0,Array(size)).map(function(el,i) {return i+1})
      var queens_columns = function(k) {
        if(!k) return [[]]
        // this naive flatmap (map + reduce(concat)) is *really* slow here, because we're cloning vast arrays
        return queens_columns(k-1).map(function(rest) {
                                    return interval.map(function(row) {return rest.concat(row) }) })
                                  .reduce(function(a,b) {return a.concat(b)}, [])
                                  .filter(function(positions) {
                                    var last = positions.slice(-1)[0]
                                    return positions.slice(0,-1).reduce(function(acc, col, index) {
                                      return acc &&
                                             !( last == col
                                             || last == col + (k - index - 1)
                                             || last == col - (k - index - 1))
                                    }, true)
                                  })
      }
      return queens_columns(size)
    }

    queens2 = function(size) {
      var interval = Array.apply(0,Array(size)).map(function(el,i) {return i+1})
      var queens_columns = function(k) {
        if(!k) return [[]]
        // here we mutate inside a reduce to simulate flatmap, which is *much* faster
        return
          queens_columns(k-1)
            .reduce(function(acc, rest) {
              interval.forEach(function(row) {acc.push(rest.concat(row)) })
              return acc}, [])
            .filter(function(positions) {
              var last = positions.slice(-1)[0]
              return positions.slice(0,-1).reduce(function(acc, col, index) {
                return acc &&
                       !( last == col
                       || last == col + (k - index - 1)
                       || last == col - (k - index - 1))
              }, true)
            })
      }
      return queens_columns(size)
    }



    document.addEventListener('DOMContentLoaded', function() {
      // var seedlikes = document.getElementById('spaceseeds').text
      // var outerseed = D.make_some_space(seedlikes)
      // OuterSpace = new D.Space(outerseed) // published for debug
    })
  </script>

</body>
</html>