module Task1_Test where

import Test.HUnit ( assertEqual, Test(TestList, TestCase) )
import Task1


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


test1 = TestCase (assertEqual "Inorder example"   [2,22,6,5,1,111,3] (values Inorder root))
test2 = TestCase (assertEqual "Postorder example" [2,6,22,111,3,1,5] (values Postorder root))
test3 = TestCase (assertEqual "Preorder example"  [5,22,2,6,1,3,111] (values Preorder root))
--test4 = TestCase (assertEqual "Empty tree"        []                 (values Preorder EmptyTree))

t1 :: Test
t1 = TestList [test1,test2,test3]
