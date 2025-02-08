### Church Encoding of Natural Numbers and Arithmetic in Lambda Calculus

#### Representation of Natural Numbers

In Church encoding, a natural number `n` is represented as a higher-order function that applies a given function `f` exactly `n` times to an argument `x`. The formal definition is: <br>
n = λf x. f<sup>n</sup> x

where function iteration follows:<br>
f<sup>0</sup> x = x,<br>
f<sup>n+1</sup> x = f(f<sup>n</sup> x)

Thus, the first few Church numerals are:

0 = λf x. x<br>
1 = λf x. f x<br>
2 = λf x. f (f x)<br>
3 = λf x. f (f (f x))<br>


#### Successor Function

The successor function is defined by applying `f` one more time to the given Church numeral:<br>

succ = λn f x. f (n f x)

which satisfies:
 
succ n = n + 1


#### Arithmetic Operations

- *Addition* <br>
    Addition can be expressed as:<br>
    add = λm n f x. m f (n f x)

    which results in:

    add m n = m + n
- *Multiplication*<br>

    Multiplication is defined as:
    mul = λm n f x. m (n f) x


    which follows:
    mul m n = m * n

- *Exponentiation*
    Exponentiation can be defined as:
    exp = λm n f. n m f


    which satisfies:
    exp m n = m<sup>n</sup>


#### Predecessor Function

The predecessor function in Church numerals is non-trivial since there is no direct way to decrement a number. Instead, we use a pair-based approach. The idea is to maintain a pair p<sub>n</sub>, p<sub>n+1</sub>, where:
(p<sub>n</sub>, p<sub>n+1</sub>) = (p<sub>n-1</sub>, p<sub>n</sub>)



We initialize with `(0, 0), then iteratively compute new pairs until we reach p<sub>n</sub>, p<sub>n+1</sub> where p<sub>n</sub> is the desired predecessor.

In lambda calculus, this can be implemented as:
pred = λn. fst (n (λx. pair (snd x) (succ (snd x))) (pair 0 0))


This ensures:<br>
pred n = n - 1


for n > 0, and 0 otherwise.

#### Subtraction and Comparison

Using the predecessor function, subtraction is defined as:
sub = λm n. n pred m

where subtraction defaults to 0 when m < n.

To check if a number is zero, we use:<br>
iszero = λn x y. n (λz. y) x 

which returns `True` if n = 0 and `False` otherwise.

The less-than-or-equal relation follows:
leq = λm n. iszero (sub m n)

which checks whether m - n = 0.

#### Ackermann Function

The Ackermann function is a recursive function defined as:
A(0, n) = n + 1
A(m+1, 0) = A(m, 1)
A(m+1, n+1) = A(m, A(m+1, n))

Its encoding in lambda calculus is:
ack = λm n. m (λf. n f (f 1)) succ

This function grows extremely fast and is not primitive recursive.
