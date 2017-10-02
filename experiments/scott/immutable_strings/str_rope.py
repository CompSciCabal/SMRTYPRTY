
class RopeString(object):
    """String using Prefix Tree (Rope)

    The prefix tree has:
     - strings in the leaf nodes
     - weight of the the left subtree at internal nodes

    """

    class LeafNode(object):
        def __init__(self, value):
            self.value = value

        def __repr__(self):
            return "LeafNode(weight={})".format(self.weight)

        @property
        def weight(self):
            return len(self.value)

        def get(self, i):
            return self.value[i]

        def __len__(self):
            return self.weight

        def __add__(self, other):
            return RopeString.Node(left=self, right=other)

        def split_at(self, i):
            # precond: i > 0 and i <= len(self)
            s1, s2 = self.value[:i], self.value[i:]
            return [RopeString.LeafNode(s1), RopeString.LeafNode(s2)]


    class Node(object):

        def __init__(self, left, right):
            self.left = left
            self.right = right
            self.weight = len(left)

        def __repr__(self):
            return "Node(weight={}, left={}, right={})".format(self.weight, self.left, self.right)

        def __len__(self):
            return self.weight + self.right.weight

        def __add__(self, other):
            # O(1) concat implementation that can lead to bad tree structure
            return RopeString.Node(left=self, right=other)

        def get(self, i):
            if i < self.weight:
                return self.left.get(i)
            else:
                return self.right.get(i - self.weight)

        def split_at(self, i):
            # We are creeping up the tree, building 2 new trees
            # by concating together the pieces on either side of
            # the split
            if i < self.weight:
                left, right = self.left.split_at(i)
                return [left, right + self.right]
            else:
                left, right = self.right.split_at(i - self.weight)
                return [self.left + left, right]


    @classmethod
    def from_node(cls, node):
        self = cls.__new__(cls)
        self.root = node
        return self

    def __init__(self, s=""):
        self.root = self.LeafNode(s)

    def __getitem__(self, position):
        if position < 0:
            raise IndexError("Index {} not found".format(position))
        return self.root.get(position)

    def __add__(self, other):
        return RopeString.from_node(self.root + other.root)

    def __eq__(self, other):
        n = len(self)
        if n != len(other):
            return False
        for i in range(n):
            if self[i] != other[i]:
                return False
        return True

    def __len__(self):
        return len(self.root)
    
    def __repr__(self):
        return ''.join([self[i] for i in range(len(self))])

    def split(self, n):
        if n == 0:
            raise ValueError("String can't be split into 0 length pieces")
        result = []
        n_total = len(self)
        position = 0
        rest = self.root
        while position < n_total:
            stride = min(n, n_total - position)
            curr, rest = rest.split_at(stride)
            result.append(RopeString.from_node(curr))
            position += n
        return result
