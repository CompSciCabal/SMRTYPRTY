
class LinkedListString(object):
    """Linked List of Strings

    Each element in the list contains a list (in C would be plain array),
    the total string is made by concatenating as you follow the list.

    This is the first example of one that aims for immutability.

    This one will start out performing well but gets worse as more and more splits and concats are performed.

    """
    class Node(object):
        def __init__(self, data, start=None, end=None, next_node=None):
            # We use explicit indices so we can avoid copying payloads
            self.start = 0 if start is None else start
            self.end = len(data) if end is None else end
            self.data = data
            self.next_node = next_node

        def __getitem__(self, position):
            return self.data[self.start + position]

        def __repr__(self):
            return "Node(data={}, start={}, end={})".format(''.join(self.data), self.start, self.end)

        def __len__(self):
            return self.end - self.start

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

    def __init__(self, s=""):
        self.head = self.Node([c for c in s])


    def __getitem__(self, position):
        if position < 0:
            raise IndexError("Index {} not found".format(position))
        curr = self.head
        n = len(curr)
        while position >= n: 
            curr = curr.next_node
            if curr is None:
                raise IndexError("Index {} not found".format(position))
            n += len(curr)
        i = len(curr) - (n - position)
        return curr[i]

    def __add__(self, other):
        # Have to copy all of the first linked list (not the data though)
        # in order to preserve immutability
        curr = self.head
        new_curr = self.Node.from_node(self.head)
        new_head = new_curr
        while curr.next_node is not None:
            curr = curr.next_node
            new_curr.next_node = self.Node.from_node(curr)
            new_curr = new_curr.next_node
        new_curr.next_node = other.head
        return LinkedListString.from_node(new_head)
        
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

    def copy_until(self, position):
        curr = self.head
        last = curr
        new_curr = self.Node.from_node(self.head)
        n = len(curr)
        while position >= n: 
            curr = curr.next_node
            if curr is None:
                break
            n += len(curr)
            last = curr
            new_curr.next_node = self.Node.from_node(curr)
            new_curr = new_curr.next_node
        i = len(last) - (n - position)
        new_curr.end = new_curr.start + i

        new_next = self.Node(data=new_curr.data,
                             start=new_curr.end,
                             end=last.end,
                             next_node=last.next_node)
        return new_curr, new_next

    def split(self, n):
        if n == 0:
            raise ValueError("String can't be split into 0 length pieces")
        result = []
        n_total = len(self)
        position = 0
        rest_string = self
        while position < n_total:
            stride = min(n, n_total - position)
            curr, rest = rest_string.copy_until(stride)
            result.append(LinkedListString.from_node(curr))
            rest_string = LinkedListString.from_node(rest)
            position += n
        return result
