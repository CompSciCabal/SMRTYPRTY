
# class StringInterface(object):
#     def concat(self, other):
#         raise Exception('Abstract Interface not callable')
# 
#     def split(self, n):
#         raise Exception('Abstract Interface not callable')


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


# Not looking to improve performance via a python implementation, but counting things would be interesting.
# Number of copies, comparisons, allocations
# First lets get it correct though

class ListString(object):
    """Implementation of String using a Python List
    """
    def __init__(self, s):
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



class BinarySearchTree(object):

    class Node(object):
        def __init__(self, key, value):
            self.left = None
            self.right = None
            self.key = key
            self.value = value

        def __repr__(self):
            return "Node(key={}, value={}, left={}, right={})".format(self.key, self.value, self.left, self.right)

        def match_and_parent(self, key, parent=None):
            if self.key == key:
                return self, parent
            elif self.key < key and self.right is not None:
                return self.right.match_and_parent(key, self)
            elif self.key > key and self.left is not None:
                return self.left.match_and_parent(key, self)
            else:
                return None, self

        def add_child(self, node):
            if self.key < node.key:
                assert self.right is None
                self.right = node
            elif self.key > node.key:
                assert self.left is None
                self.left = node
            else:
                raise ValueError('Adding child with equal key')

        def remove_child(self, node):
            if node is self.left:
                self.left = None
            elif node is self.right:
                self.right = None
            else:
                raise ValueError("Not this node's child")

    def __init__(self):
        self.root = None
        self.size = 0

    def get(self, key):
        if self.root is None:
            raise IndexError('Key {} not Found'.format(key))
        node, _ = self.root.match_and_parent(key)
        if node is None:
            raise IndexError('Key {} not Found'.format(key))
        return node.value

    def set(self, key, value):
        if self.root is None:
            self.root = self.Node(key, value)
            self.size += 1
            return
        node, parent = self.root.match_and_parent(key)
        if node is None:
            node = self.Node(key, value)
            parent.add_child(node)
            self.size += 1
        else:
            node.value = value

    def remove(self, key):
        if self.root is None:
            raise IndexError('Key {} not Found'.format(key))
        node, parent = self.root.match_and_parent(key, self.root)
        if node is None:
            raise IndexError('Key {} not Found'.format(key))
        elif node is parent:
            self.root = None
            self.size = 0
        else:
            parent.remove_child(node)
            self.size -= 1

    def __len__(self):
        return self.size

    def __eq__(self, other):
        if self.root is None and other.root is None:
            return True
        elif self.root is None or other.root is None:
            return False
        elif len(self) != len(other):
            return False
        for i in range(len(self)):
            a = self.get(i)
            b = other.get(i)
            if a != b:
                return False
        return True


class BSTString(object):
    """Implementation of String using a binary search tree

    This is pretty ineffective for everything, is intended as
    using a barebones data structure for a string implementation
    that isn't trivial like List.
    """
    def __init__(self, s):
        self._data = BinarySearchTree()
        for i, c in enumerate(s):
            self._data.set(i, c)

    def __getitem__(self, position):
        return self._data.get(position)

    def __add__(self, other):
        result = BSTString("")
        n = len(self)
        for i in range(n):
            result._data.set(i, self[i])
        for i in range(n, n + len(other)):
            result._data.set(i, other[i - n])
        return result

    def __eq__(self, other):
        return self._data == other._data

    def __len__(self):
        return len(self._data)
    
    def __repr__(self):
        return ''.join([self._data.get(i) for i in range(len(self))])

    def split(self, n):
        indices = [i for i in range(len(self))]
        index_list = [indices[i:i+n] for i in range(0, len(self), n)]
        result = []
        for indices in index_list:
            result.append(BSTString([self._data.get(i) for i in indices]))
        return result


class LinkedListString(object):
    """Linked List of Strings

    Each element in the list contains a list (in C would be plain array),
    the total string is made by concatenating as you follow the list.

    This is the first example of one that aims for immutability.

    This one will start out performing well but gets worse as more and more splits and concats are performed.

    """
    class Node(object):
        def __init__(self, data, start=None, end=None):
            # We use explicit indices so we can avoid copying payloads
            self.start = 0 if start is None else start
            self.end = len(data) if end is None else end
            self.data = data
            self.next_node = None

        def __getitem__(self, position):
            return self.data[self.start + position]

        def __repr__(self):
            return "Node(data={}, start={}, end={})".format(self.data, self.start, self.end)

        def __len__(self):
            return self.end - self.start

        def get_tail(self):
            if self.next_node is None:
                return self
            return self.next_node.get_tail()
        
        @classmethod
        def from_node(cls, node):
            self = cls.__new__(cls)
            self.__init__(node.data, node.start, node.end)
            return self


    @classmethod
    def from_node(cls, node):
        self = cls.__new__(cls)
        self.head = node
        return self

    def __init__(self, s):
        self.head = self.Node([c for c in s])


    def __getitem__(self, position):
        curr = self.head
        n = len(curr)
        while position >= n: 
            curr = curr.next_node
            if curr is None:
                raise IndexError("Index {} not found".format(position))
            n += len(curr)
        return curr[position - n]

    def __add__(self, other):
        # Have to copy all of the first linked list (not the data though)
        # in order to preserve immutability
        curr = self.head
        new_curr = self.Node.from_node(self.head)
        while curr.next_node is not None:
            curr = curr.next_node
            new_curr.next_node = self.Node.from_node(curr)
            new_curr = new_curr.next_node
        new_curr.next_node = other.head
        return LinkedListString.from_node(new_curr)
        
    def __eq__(self, other):
        n = len(self)
        if n != len(other):
            return False
        for i in range(n):
            if self[i] != other[i]:
                return False
        return True

    def __len__(self):
        acc = 0
        curr = self.head
        while curr is not None:
            acc += len(curr)
            curr = curr.next_node
        return acc
    
    def __repr__(self):
        return ''.join([self[i] for i in range(len(self))])

    def split(self, n):
        if n == 0:
            raise ValueError("String can't be split into 0 length pieces")
        # TODO:: We should be able to split by just manipulating indices and not doing any extra data copies
        # could be plenty of linked list node copies though
        return []
        


        

