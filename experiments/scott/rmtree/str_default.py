# Not looking to improve performance via a python implementation, but counting things would be interesting.
# Number of copies, comparisons, allocations
# First lets get it correct though


class String(str):
    """Default Implementation
    
    Uses python immutable strings.
    Important overrides:
      __getitem__ : indexing
      __add__     : concat
      __eq__      : equal
      split       : split

    Useful overrides:
      __init__
      __len__
      __iter__
      __repr__
    """
    def __getitem__(self, position):
        # Disabling negative indexing
        if (position < 0) or (position >= len(self)):
            raise IndexError("String doesn't support magic indexing")
        return super(String, self).__getitem__(position)

    def __add__(self, other):
        return String(super(String, self).__add__(other))

    def split(self, n):
        return [String(super(String, self).__getitem__(slice(i, i+n)))
                for i in range(0, len(self), n)]
