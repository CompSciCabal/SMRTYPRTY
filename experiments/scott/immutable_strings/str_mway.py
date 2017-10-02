"""
http://hypirion.com/musings/understanding-persistent-vector-pt-1
32 wide Tree
append  : O(1)
indexing: log_32 N  -> 1/5 lg N
updates : 32/5 lg N
concat  : O(N)
split   : O(N) 
"""
from math import log


class MWayString(object):
    """String using array mapped Tree (MWay)

    Two Parameters to tune the size of the tree
     - BranchingFactor is how many children at internal nodes
     - ArrayWidth is the length of the array at leafs


    """
    BranchingFactor = 4
    ArrayWidth = 4

    @staticmethod
    def child_capacity(depth):
        n = MWayString.ArrayWidth
        m = MWayString.BranchingFactor
        child_capacity_leafs = m ** (depth - 1)
        return child_capacity_leafs * n

    class LeafNode(object):
        @classmethod
        def copy(cls, node):
            self = cls.__new__(cls)
            self._data = [value for value in node._data]
            return self

        def __init__(self):
            self._data = ["x" for i in range(MWayString.ArrayWidth)]

        def mutating_set(self, depth, position, value):
            assert depth == 0
            self._data[position] = value

        def get(self, depth, position):
            assert depth == 0
            return self._data[position]

        def surface_copy(self, depth, position):
            assert depth == 0
            return MWayString.LeafNode.copy(self)


    class Node(object):
        @classmethod
        def copy(cls, node):
            self = cls.__new__(cls)
            self.children = [child for child in node.children]
            return self

        def __init__(self, child=None):
            self.children = []
            if child is not None:
                self.children.append(child)

        def _child_index_and_position(self, depth, position):
            child_capacity = MWayString.child_capacity(depth)

            child_index = position // child_capacity
            child_position = position % child_capacity
            return child_index, child_position

        def mutating_set(self, depth, position, value):
            child_index, child_position = self._child_index_and_position(depth, position)
            if child_index == len(self.children):
                self.children.append(MWayString.make_node_at_depth(depth))

            self.children[child_index].mutating_set(depth - 1, child_position, value)

        def get(self, depth, position):
            child_index, child_position = self._child_index_and_position(depth, position)

            return self.children[child_index].get(depth - 1, child_position)

        def surface_copy(self, depth, position):
            child_index, child_position = self._child_index_and_position(depth, position)

            new_self = MWayString.Node.copy(self)
            child = self.children[child_index]
            new_child = child.surface_copy(depth - 1, child_position)
            new_self.children[child_index] = new_child
            # Deleting any later children, makes this child the
            # end of the current tree
            for i in range(child_index + 1, len(self.children)):
                del new_self[i]
            return new_self


    @staticmethod
    def make_node_at_depth(depth):
        if depth == 1:
            return MWayString.LeafNode()
        else:
            return MWayString.Node()


    @staticmethod
    def child_capacity(depth):
        n = MWayString.ArrayWidth
        m = MWayString.BranchingFactor
        child_capacity_leafs = m ** (depth - 1)
        return child_capacity_leafs * n

    @staticmethod
    def largest_bounding_multiple(l, n):
        # return x where x is the largest multiple of n s.t. x >= l
        return ((l + n - 1) // n) * n

    @classmethod
    def from_root(cls, root, size, capacity):
        self = cls.__new__(cls)
        self.root = root
        self.size = size
        self.capacity = capacity
        return self

    def __init__(self, s=""):
        self.size = 0
        self.root = self.LeafNode()
        self.capacity = self.ArrayWidth
        for i in range(len(s)):
            self._mutating_append(s[i])

    @property
    def depth(self):
        n = MWayString.ArrayWidth
        m = MWayString.BranchingFactor

        capacity_leafs = self.capacity // n

        assert capacity_leafs % m == 0 or capacity_leafs % m == 1
        return int(log(capacity_leafs, m))

    def __len__(self):
        return self.size

    def __eq__(self, other):
        n = len(self)
        if n != len(other):
            return False
        for i in range(n):
            if self[i] != other[i]:
                return False
        return True

    def __repr__(self):
        return ''.join([self[i] for i in range(len(self))])

    def _mutating_append(self, value):
        if len(self) == self.capacity:
            self.root = self.Node(self.root)
            self.capacity = self.capacity * self.BranchingFactor
        self.root.mutating_set(self.depth, len(self), value)
        self.size += 1

    def append(self, value):
        other = MWayString([value])
        return self + other

    def __getitem__(self, position):
        if position < 0:
            raise IndexError("Index {} not found".format(position))
        return self.root.get(self.depth, position)

    def __add__(self, other):
        root = self.root.surface_copy(self.depth, len(self) - 1)
        result = MWayString.from_root(root, self.size, self.capacity)

        for i in range(len(other)):
            result._mutating_append(other[i])
        return result


    def split(self, n):
        if n == 0:
            raise ValueError("String can't be split into 0 length pieces")
        result = []
        n_total = len(self)
        position = 0
        while position < n_total:
            stride = min(n, n_total - position)
            # TODO:: Better implementation ??
            curr = MWayString([self[i + position] for i in range(stride)])
            result.append(curr)

            position += stride
        return result
