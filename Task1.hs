module Task1 where
data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)


-- Inorder (Left, Root, Right) 
-- Preorder (Root, Left, Right)
-- Postorder (Left, Right, Root)

--Example tree:
node7 :: Tree Int
node7 = Node {value = 111, left = EmptyTree, right = EmptyTree}

node6 :: Tree Int
node6 = Node {value = 3, left = node7, right = EmptyTree}

node5 :: Tree Int
node5 = Node {value = 6, left = EmptyTree, right = EmptyTree}

node4 :: Tree Int
node4 = Node {value = 2, left = EmptyTree, right = EmptyTree}

node3 :: Tree Int
node3 = Node {value = 1, left = EmptyTree, right = node6}

node2 :: Tree Int
node2 = Node {value = 22, left = node4, right = node5}

root :: Tree Int
root = Node {value = 5, left = node2, right = node3}



values :: Strategy -> Tree a -> [a]
values _ EmptyTree = []
values Inorder   t = values Inorder (left t) ++ value t : values Inorder (right t)
values Postorder t = values Postorder (left t) ++ values Postorder (right t) ++ [value t]
values Preorder  t = value t : values Preorder (left t) ++ values Preorder (right t)