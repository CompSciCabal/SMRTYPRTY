# Simple Trie Implementation to get a feel for things
# uses reference counters to track where strings end and if there are multiple copies of the same string


class Node:
    def __init__(self):
        self.children = dict()
        self.count = 0
    
    def incrementReferenceCount(self):
        self.count = self.count + 1
        
    def decrementReferenceCount(self):
        self.count = self.count - 1
     
    def addChild(self, value):
        self.children[value] = Node()
        
    def getChild(self, value):
        return(self.children[value])
        
    def hasChild(self, value):
        return(value in self.children)
        
    def deleteChild(self, value):
        del(self.children[value])
        
    def isTerminal(self):
        refs = 0
        for v in self.children.values():
            refs = refs + v.count
        return refs < self.count
    
class Trie:
    def __init__(self):
        self.root = Node()
        
    def has(self, value):
        n = self.root
        for v in value:
            if n.hasChild(v):
                n = n.getChild(v)
            else:
                return False
        return n.isTerminal()
        
    def insert(self, value):
        n = self.root
        n.incrementReferenceCount()
        for v in value:
            if not n.hasChild(v):
                n.addChild(v)
            n = n.getChild(v)
            n.incrementReferenceCount()
            
    def delete(self, value):
        n = self.root
        L = []
        for v in value:
            if n.hasChild(v):
                n = n.getChild(v)
                L.append((v,n))
            else:
                return
        
        # Iterate backwards over the nodes, deleting all leaves as they are encountered
        last = None
        for k,n in reversed(L):
            n.decrementReferenceCount()
            if last is not None:
                lk, ln = last
                if ln.count == 0:
                    n.deleteChild(lk)
            last = (k,n)
        self.root.decrementReferenceCount()


def assertTrue(expression, message):
    if not expression:
        print(message)


if __name__ == "__main__":
    n = Node()
    n.addChild('a')
    if n.hasChild('a'):
        print("node initialized correctly")
        
    t = Trie()
    t.insert("hello")
    assertTrue(not t.has("not hello"), "Failed to not find not hello")
    assertTrue(t.has("hello"), "Failed to find hello")
    
    t.insert("hell on wheels")
    assertTrue(t.has("hell on wheels"), "Failed to find hell on wheels")
    assertTrue(not t.has("hell"), "Failed to not find hell")
    
    t.delete("hello")
    assertTrue(t.has("hell on wheels"), "Failed to find hell on wheels after deleting hello")
    assertTrue(not t.has("hello"), "Failed to not find hello after deleting hello")
    
    t.insert("hello")
    t.insert("hello")
    assertTrue(t.has("hell on wheels"), "Failed to find hell on wheels after reinserting two hellos")
    assertTrue(t.has("hello"), "Failed to find hello after reinserting hello")
    
    t.delete("hello")
    assertTrue(t.has("hell on wheels"), "Failed to find hell on wheels after reinsert and delete one hellos")
    assertTrue(t.has("hello"), "Failed to find hello after reinsert and delete one hello")
    
    t.delete("hello")
    assertTrue(t.has("hell on wheels"), "Failed to find hell on wheels after deleting both hello")
    assertTrue(not t.has("hello"), "Failed to not find hello after deleting both hello")
    
    t.delete("hell on wheels")
    assertTrue(not t.has("hell on wheels"), "Failed to not find hell on wheels after deleting all")
    assertTrue(not t.has("hello"), "Failed to not find hello after deleting all")
    
    
