data Trie a = Leaf a | Node a [Trie a]

main :: IO ()
main = do
    let 
        t = Node 'c' [
            Node 'a' [Leaf 'r', Leaf 't'],
                Node 'o' [
                    Node 'o' [
                        Leaf 'l'
                    ]
                ]
            ]
    print "Hello"