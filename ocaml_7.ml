# HW7: Induction

## Question 1: Induction on lists

### Question 1.a
Induction on ls.

CASE: ls = []

WTS: a + sum_tr [] acc = sum_tr [] (a + acc)

LHS: a + sum_tr [] acc
= a + acc -- by definition of sum_tr
RHS: sum_tr [] (a + acc)
= a + acc -- by definition of sum_tr
LHS = RHS

Inductive Step:

Assume the statement holds for a list ls, i.e.,
a + sum_tr ls acc = sum_tr ls (a + acc)
We need to prove it for the list x :: ls.

LHS: a + sum_tr (x :: ls) acc
= a + sum_tr ls (x + acc) -- by definition of sum_tr
Using the inductive hypothesis on the term sum_tr ls (x + acc):
= a + sum_tr ls (x + a + acc) -- by associativity of addition
RHS: sum_tr (x :: ls) (a + acc)
= sum_tr ls (x + a + acc) -- by definition of sum_tr
LHS = RHS

Done.



### Question 1.b

Prove that
If `l1,l2 : int list` and `acc : int` then
`sum_tr (l1 @ l2) acc = sum_tr l1 (sum_tr l2 acc)`
Induction on l1.

CASE: l1 = []

WTS: sum_tr ([] @ l2) acc = sum_tr [] (sum_tr l2 acc)

LHS: sum_tr ([] @ l2) acc
= sum_tr l2 acc -- by definition of list concatenation
RHS: sum_tr [] (sum_tr l2 acc)
= sum_tr l2 acc -- by definition of sum_tr
LHS = RHS

Inductive Step:

Assume the statement holds for a list l1, that is:
sum_tr (l1 @ l2) acc = sum_tr l1 (sum_tr l2 acc)
(This is our Inductive Hypothesis.)
We need to prove it for the list x :: l1.

LHS: sum_tr ((x :: l1) @ l2) acc
= sum_tr (x :: (l1 @ l2)) acc -- by definition of list concatenation
= sum_tr (l1 @ l2) (x + acc) -- by definition of sum_tr
Applying the inductive hypothesis:
= sum_tr l1 (sum_tr l2 (x + acc)) -- by the inductive hypothesis
RHS: sum_tr (x :: l1) (sum_tr l2 acc)
= sum_tr l1 (x + sum_tr l2 acc) -- by definition of sum_tr
By associativity of addition:
= sum_tr l1 (sum_tr l2 (x + acc)) -- rearranging the sum
LHS = RHS

Done.




### Question 1.c

Prove that
If `ls : int list` and `acc : int` then
`acc + sum ls = sum_tr (rev ls) acc`
Base Case: When ls = [].

Left-hand side (LHS):
acc + sum [] = acc (since sum of an empty list is 0).

Right-hand side (RHS):
sum_tr (rev []) acc = sum_tr [] acc = acc.

Clearly, LHS = RHS for the base case.

Inductive Step:

Assume the statement is true for a list ls of length n, i.e.,
acc + sum ls = sum_tr (rev ls) acc.

Now, we prove for a list x :: ls of length n+1.

LHS:
acc + sum (x :: ls) = acc + x + sum ls
Using our inductive hypothesis for the term sum ls, we can rewrite this as:
x + sum_tr (rev ls) acc

RHS:
sum_tr (rev (x :: ls)) acc
Using the definition of rev, this is equal to:
sum_tr (rev ls @ [x]) acc

Now, applying the first given property on rev ls and [x]:
sum_tr (rev ls) (sum_tr [x] acc)

Given the definition of sum_tr, sum_tr [x] acc = x + acc. Plugging this in, we get:
sum_tr (rev ls) (x + acc)

Now, applying the second given property:
x + sum_tr (rev ls) acc

So, LHS = RHS for the list x :: ls.

## Question 2
Prove that 
If `t : tree` then `height t = height' t`
 
Base Case: When t is Empty

For height function:
height of Empty = 0

For height' function:
height' of Empty = 0

It's clear that for the base case, height of Empty = height' of Empty.

Inductive Step:

Assume the statement is true for trees t1 and t2. That is, we assume:
height of t1 = height' of t1
and
height of t2 = height' of t2

We aim to prove the statement for the composite tree Node(t1, t2).

For the height function:
height of Node(t1, t2) = 1 + maximum of [height of t1, height of t2]

Using our assumption:
= 1 + maximum of [height' of t1, height' of t2]

Now, using the provided lemma: For any integers a, b, and x,
x + maximum of [a, b] = maximum of [a + x, b + x]

Letting x = 1, a = height' of t1, and b = height' of t2, we get:
= maximum of [height' of t1 + 1, height' of t2 + 1]

But from the definition of height' function, this is:
= height' of Node(t1, t2)

Thus,  
height of Node(t1, t2) = height' of Node(t1, t2)



## Question 3

Prove that
If `t : int tree` then
`inorder_traversal_1 t = inorder_traversal_2 t []`


1. Base Case: When t is Empty
For th function inorder_traversal_1:
Result on Empty = []
For the function inorder_traversal_2:
Result on Empty with [] as the accumulator = []
It's clear that in both cases, when the tree is empty, the result is an empty list.
2. Inductive Step:
assume the statement holds true for trees t1 and t2
Result of inorder_traversal_1 t1 = Result of inorder_traversal_2 t1 []
Result of inorder_traversal_1 t2 = Result of inorder_traversal_2 t2 []
Now, consider a tree Node(t1, x, t2).
For the function inorder_traversal_1:
Result on Node(t1, x, t2) = (Result on t1) followed by x followed by (Result on t2)
Using our assumption:
Result of inorder_traversal_2 t1 [] followed by x followed by Result of inorder_traversal_2 t2 []

For the function inorder_traversal_2:
Result on Node(t1, x, t2) with [] as the accumulator = Result on t1 when the accumulator is x followed by the result of traversing t2 with an empty list as the accumulator.
By applying associativity of list concatenation and our inductive assumption, we can infer that the result of the traversal using both functions is the same for Node(t1, x, t2).

Thus, by induction, we've shown that for every tree t, the result of inorder_traversal_1 t is equivalent to the result of inorder_traversal_2 t [].

