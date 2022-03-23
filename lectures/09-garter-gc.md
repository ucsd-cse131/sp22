---
title: Garbage Collection
date: 2018-03-7
headerImg: fox.jpg
---

## Adding Garbage Collection to the Runtime

![Compiler and Runtime](/static/img/compiler-pipeline-1-2.png)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ: Tuples on the Heap

Using the above "library" we can write code like:

```haskell
def tup4(x1, x2, x3, x4):
  (x1, (x2, (x3, (x4, false))))

def get(e, i):
  if (i == 0):
    head(e)
  else:
    get(tail(e), i-1)

let quad = tup4(1, 2, 3, 4) in
  get(quad, 0) + get(quad, 1) + get(quad, 2) + get(quad, 3)
```

What will be the result of compiling the above?

1. Compile error
2. Segmentation fault
3. Other run-time error
4. `4`
5. `10`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## QUIZ

Using the above "library" we can write code like:

```haskell
let quad = tup4(1, 2, 3) in
  get(quad, 0) + get(quad, 1) + get(quad, 2) + get(quad, 3)
```

What will be the result of compiling the above?

1. Compile error
2. Segmentation fault
3. Other run-time error
4. `4`
5. `10`


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Example 1: Garbage at End

```python
let x = (1, 2)
  , y = let tmp = (10, 20)
        in tmp[0] + tmp[1]
in
  (x[0] + y, x[1] + y)
```

[Click to see video][mov1]

<iframe src="https://www.youtube.com/embed/nRGftMYg_3g" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Example 2: Garbage in Middle

```python
let y = let tmp = (10, 20)
        in tmp[0] + tmp[1]
  , x = (1, 2)

in
  (x[0] + y, x[1] + y)
```

[Click to see video][mov2]

<iframe src="https://www.youtube.com/embed/sDlfN7n-4ws" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



## Example 3: Garbage in Middle (with stack)

```python
def foo(p, q):
  let tmp = (10, 20)
  in tmp[0] + tmp[1]

let y  = foo(10, 20)
  , x  = (y, y + 1)
  , z  = foo(100, 200)
in
  x[0] + z
```

[Click to see video][mov3]

<iframe src="https://www.youtube.com/embed/FQUWO_WfP2s" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Example 4: Transitive Reachability

```python
def range(i, j):
  if (i < j):
    (i, range(i+1, j))
  else:
    false

def sum(l):
  if l == false:
    0
  else:
    l[0] + sum(l[1])

let t1 =
         let l1 = range(0, 3)
         in sum(l1)
  , l  = range(t1, t1 + 3)
in
  (1000, l)
```

[Click to see video][mov4]

<iframe src="https://www.youtube.com/embed/25L_-VQUQ9s" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

## Postlude

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## What will *you learn* ?

**Core principles of compiler construction**

* Syntax Trees
* Type Checking (oops!)
* Intermediate forms
* Optimization
* Closures
* Calling conventions (Stacks) 
* Memory management (Heap)
* Garbage collection

**Several new languages**

* `Haskell` to write the compiler
* `C`       to write the "run-time"
* `X86`     compilation target

**How to write a large program**

* How to use types for design
* How to add new features / refactor
* How to test & validate   

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## This is just the beginning ...

This is a *tiny* fraction of **what we know about** compilers

### Optimizations 

- Many many more optimizations
- See this https://www.lihaoyi.com/post/HowanOptimizingCompilerWorks.html

### Type checking and Static analysis

- Fast user-interfaces for error reporting
- [Anders Hejlsberg on Modern Compiler Construction](https://www.youtube.com/watch?v=wSdV1M7n4gQ)
- [Checker framework](https://checkerframework.org/)

### Instrumenting code for debugging 

- https://rr-project.org/

### Just-in-time (JIT) Compilation 

- V8, Spidermonkey,...
- [Torque](https://v8.dev/docs/torque)

### Formal semantics and Correctness 

- (Compcert, CakeML, ...)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## ... and many new things are still to be invented!

### Optimization 

How to generate the *fastest possible* code

- Image processing with [Halide](https://halide-lang.org/)
- Finding the best sequence of instructions [Superoptimization](https://www.youtube.com/watch?v=Ux0YnVEaI6A)
- Optimizing [Tensor Computations](https://www.tensorflow.org/xla)

### Verification

How to get the machine to *check* your code 

- Contracts with [Dafny](https://dafny-lang.github.io/dafny/)
- Refinement types [LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell-blog/)
- Theorem provers [Coq](https://softwarefoundations.cis.upenn.edu/), [Isabelle](https://isabelle.in.tum.de/), [Lean](https://leanprover.github.io/)

### Synthesis 

How to get the machine to *write* your code 

- Using examples [Flashfill](https://www.youtube.com/watch?v=w-k9WjRJvIY)
- Using fancy types [Synquid](https://www.youtube.com/watch?v=HnOix9TFy1A)
- Using live programming [Snippy](https://www.youtube.com/watch?v=6xmCPznThN8)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Compilers touches pretty much all of CS ...

**Theory**

- Regular expressions, Parsing, Optimizations, Graph algorithms

**HCI**

- Interacting with the PL/Compiler
- Type annotations
- Error messages and reporting 

**HW/Architecture**

- duh.

**Machine Learning**

- "Tensorflow" 
- Optimizing ML computations
- Differentiable programming
- PL for ML [Spark](https://spark.apache.org/)
- HW and compilers for ML [OctoML](https://octoml.ai/)

**Security**

- Compiler bugs == Security vulnerabilities
- Buffer overflows, Heap spraying, JIT based attacks ...
- "Return-oriented Programming"

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


<!-- NEW --> 

[mov1]: https://youtu.be/nRGftMYg_3g
[mov2]: https://youtu.be/sDlfN7n-4ws 
[mov3]: https://youtu.be/FQUWO_WfP2s
[mov4]: https://youtu.be/25L_-VQUQ9s


<!-- OLD (32-bit) 
[mov1]: https://youtu.be/LddzswC7jEM
[mov2]: https://youtu.be/2u0y-u1ksbY
[mov3]: https://youtu.be/pCrBJWDwr2k
[mov4]: https://youtu.be/3MEmwxtEprE 
-->


