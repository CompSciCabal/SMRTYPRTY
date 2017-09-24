
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
    def __init__(self, s=""):
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
