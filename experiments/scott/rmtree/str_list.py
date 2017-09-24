
class ListString(object):
    """Implementation of String using a Python List
    """
    def __init__(self, s=""):
        self._data = [c for c in s]

    def __getitem__(self, position):
        if (position < 0) or (position >= len(self)):
            raise IndexError("String doesn't support magic indexing")
        return self._data[position]

    def __add__(self, other):
        return ListString(self._data + other._data)

    def __eq__(self, other):
        return self._data == other._data

    def __len__(self):
        return len(self._data)
    
    def __repr__(self):
        return ''.join(self._data)

    def split(self, n):
        return [ListString(self._data[i:i+n])
                for i in range(0, len(self), n)]
