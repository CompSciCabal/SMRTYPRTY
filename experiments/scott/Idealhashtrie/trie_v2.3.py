#Simple Trie implementation
#Can specify a hash function to hash the keys first
#Includes reference counting, but for simplicity only saving 1 value per node

class Node:
    def __init__(self):
        self.children = dict()
        self.count = 0
        self.value = None
    
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

#Function to use as the identity with iterable keys
def identity(x):
    return x    

from struct import pack  
#pack transforms into a byte array, which is a list of the 
#full size of the integer type, where each entry is an unsigned integer 
#in the range 0 - 255  
#This is not 32 so our trees will be wider
def hash_256(key):
    return pack('q', hash(key))

#We can additionally fiddle with the packed array to get them into, bitwise blocks
#of whatever size we want, i.e. 32 if we want to use the 5 bit key segments like in the paper   
    
class Trie:
    def __init__(self, hashfn = identity):
        self.root = Node()
        self.hash = hashfn
        
    def has(self, key):
        n = self._get(key)
        if n is None:
            return False
        return n.isTerminal()
        
    def _get(self, key):
        n = self.root
        key = self.hash(key)
        for k in key:
            if n.hasChild(k):
                n = n.getChild(k)
            else:
                return None
        return n
        
    def get(self, key):
        n = self._get(key)
        if n is None:
            #TODO: return an exception here, same as python dict
            return None
        return n.value
        
    def insert(self, key, value = None):
        n = self.root
        key = self.hash(key)
        n.incrementReferenceCount()
        for k in key:
            if not n.hasChild(k):
                n.addChild(k)
            n = n.getChild(k)
            n.incrementReferenceCount()
        n.value = value
            
    def delete(self, key):
        n = self.root
        key = self.hash(key)
        L = []
        for k in key:
            if n.hasChild(k):
                n = n.getChild(k)
                L.append((k,n))
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
        
    t = Trie(hash_256)
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
    
    t.insert("new key", "new value")
    print(t.get("new key"))
    t.delete("new key")
    print(t.get("new key"))
       
        
