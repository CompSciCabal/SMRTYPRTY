import unittest
from itertools import product

from .rm_strings import String, ListString, BSTString, LinkedListString


class TestDefault(unittest.TestCase):
    def setUp(self):
        self.String = String

    def test_indexing(self):
        s = "some string"
        my_s = self.String(s)
        for i, c in enumerate(s):
            assert c == my_s[i]
        
        with self.assertRaises(IndexError):
            my_s[-1]
        with self.assertRaises(IndexError):
            my_s[32]

    def run_concat_test(self, desc, a, b, expected):
        a = self.String(a)
        b = self.String(b)
        expected = self.String(expected)
        a_before = a
        b_before = b

        result = a + b
        assert isinstance(result, self.String), desc
        assert result == expected, desc
        assert a == a_before, desc
        assert b == b_before, desc

    def test_concat_simple(self):
        cases = [
            ("empty and empty", "", "", ""),
            ("empty and normal", "", "some string", "some string"),
            ("normal and empty", "some string", "", "some string"),
            ("normal and normal", "some", " string", "some string")]
        for desc, a, b, expected in cases:
            self.run_concat_test(desc, a, b, expected)

    def test_concat_small_lengths(self):
        n = 16
        cases = [("len = {} and len = {}".format(i, j),
                  "a" * i,
                  "b" * j,
                  "a" * i + "b" * j)
                 for i, j in product(range(n), range(n))]
        for desc, a, b, expected in cases:
            self.run_concat_test(desc, a, b, expected)

    def run_split_test(self, desc, a, n):
        my_a = self.String(a)
        a_before = my_a
        expected = [self.String(a[i:i+n]) for i in range(0, len(a), n)] 

        result = my_a.split(n)
        assert isinstance(result, list), desc
        assert result == expected, desc
        assert a_before == my_a, desc
    
    def test_split_simple(self):
        cases = [("empty and 1", "", 1),
                 ("empty and -1", "", -1),
                 ("normal and 1", "some string", 1),
                 ("normal and 2", "some string", 2),
                 ("normal and 3", "some string", 3),
                 ("normal and longer than string", "some string", 32)]
        for desc, a, n in cases:
            self.run_split_test(desc, a, n)

    def test_split_error(self):
        cases = [("empty and 0", "", 0),
                 ("normal and 0", "some string", 0)]
        for desc, a, n in cases:
            with self.assertRaises(ValueError):
                self.String(a).split(n)

    def run_split_join_test(self, desc, a, n):
        my_a = self.String(a)
        split = my_a.split(n)
        joined = self.String()
        for s in split:
            joined = joined + s
        assert joined == my_a, desc

    def run_sjsj_test(self, desc, a, n):
        my_a = self.String(a)
        split = my_a.split(n)
        joined = self.String()
        for s in split:
            joined = joined + s
        assert joined == my_a, desc

        split_2 = joined.split(n)
        joined_2 = self.String()
        for s in split:
            joined_2 = joined_2 + s
        assert joined_2 == my_a, desc

    def test_split_join(self):
        cases = [("normal and 1", "some string", 1),
                 ("normal and 2", "some string", 2),
                 ("normal and 3", "some string", 3),
                 ("normal and longer than string", "some string", 32)]
        for desc, a, n in cases:
            self.run_split_join_test(desc, a, n)

        for desc, a, n in cases:
            self.run_sjsj_test(desc, a, n)

class TestListString(TestDefault):
    def setUp(self):
        self.String = ListString

class TestBSTString(TestDefault):
    def setUp(self):
        self.String = BSTString

class TestLinkedListString(TestDefault):
    def setUp(self):
        self.String = LinkedListString

