---
title: First Class Functions
date: 2021-5-20
headerImg: fer-de-lance.jpg
---


## Functions as Values

Consider the following `egg` program

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```

What will be the result of compiling/running?

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

We have functions, but they are *second-class* entities
in our languages: they don't have the same *abilities*
as other values.

So, we get multiple error messages:

```
Errors found!
tests/input/hof.diamond:(2:3)-(4:1): Function 'it' is not defined

         2|    it(5)
               ^^

tests/input/hof.diamond:7:3-7: Unbound variable 'incr'

         7|  f(incr)
               ^^^^
```

This is because the `Env` only holds

- parameters, and
- let-bound variables

and **not** function definitions.

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


## Functions as Values

But for the many reasons we saw in CSE 130 -- we *want* to treat functions
like values. For example, if you run the above in Python you get:

```python
>>> def f(it): return it(5)

>>> def incr(x): return x + 1

>>> f(incr)
6
```

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

## Flashback: How do we _compile_ `incr`?

We compile each function down into a sequence of instructions
corresponding to its body.

```python
def incr(x):
  x + 1

incr(5)
```

becomes, for `incr`

```nasm
label_def_incr_start:
  push rbp                 # setup stack frame
  mov rbp, rsp

  mov rax, rdi             # grab param
  add rax, 2               # incr by 1

  mov rsp, rbp             # undo stack frame
  pop rbp
  ret                      # buh-bye
```

for the main expression

```nasm
our_code_starts_here:         
  push rbp
  mov rbp, rsp

  mov  rdi 10                 # push arg '5'
  call label_def_incr_start   # call function

  mov rsp, rbp
  pop  rbp
  ret
```

## What is the _value_ of a function?

So now, lets take a step back. Suppose we want to compile

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```


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


## Attempt 1: What is the value of the parameter `it` ?

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
<br>

**IDEA:** Use the **label** where `incr` lives!

```nasm
label_def_f_start:
  push rbp
  mov  rbp, rsp

  mov rax, rdi       # grab function-address
  mov rdi, 10        # push arg '5'
  call rax           # call function!

  mov rsp, rbp
  pop rbp
  ret
```

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
<br>

## How to pass the value of the parameter ?

So now the `main` expression

```python
f(incr)
```

can be compiled to:

```nasm
our_code_starts_here:         
  push rbp
  mov rbp, rsp

  mov rdi, ?1     # push arg
  call ?2         # call function

  mov rsp, rbp
  pop rbp
  ret
```

**QUIZ:** What are suitable terms for `?1` and `?2` ?

|        | `?1`                   |   `?2`                 |
|-------:|:-----------------------|:-----------------------|
| **A**  | `label_def_incr_start` | `label_def_f_start`    |
| **B**  | `label_def_f_start`    | `label_def_incr_start` |
| **C**  | `label_def_f_start`    | `label_def_f_start`    |
| **D**  | `label_def_incr_start` | `label_def_incr_start` |


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


## Strategy Progression

1. **Representation** = `Start-Label`

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
<br>
<br>

    - **Problem:** How to do run-time checks of valid args?

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


## Yay, that was easy! How should the following behave?

```python
def f(it):
  it(5)

def add(x, y):
  x + y

f(incr)
```

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

Lets see what Python does:

```python
>>> def f(it): return it(5)
>>> def add(x,y): return x + y
>>> f(add)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 1, in f
TypeError: add() takes exactly 2 arguments (1 given)
```

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

## Problem: Ensure Valid Number of Arguments?

How to make sure

* `f(incr)` **succeeds**, but
* `f(add)`  **fails**

With **proper run-time error**?

1. **Where** does the run-time check happen?
2. **What** information is needed for the check?

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


**Key:** Need to _also_ store the function's **arity**

- The **number of arguments** required by the function

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

## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

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

## Attempt 2: What is the value of the parameter `it` ?

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```

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

**IDEA:** Represent a *function* with a **tuple of** 

```
(arity, function_start_label)
```

<br>
<br>
<br>

We can now compile a call

```
e(x1,...,xn)
```

via the following strategy:

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


1. **Evaluate** the tuple `e`
2. **Check** that `e[0]` is equal to `n` (else arity mismatch error)
3. **Call** the function at `e[1]`

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



### Example

Lets see how we would compile this

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```

We need to map **the variable**

  `incr`

to **the tuple**

  `(1, label_def_incr_start)`

But **where** will we store this information?

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


## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

    - **Problem:** How to map function **names** to tuples?

3. **Lambda Terms** Make functions just another expression!

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


## Attempt 3: Lambda Terms

So far, we could only define functions at **top-level**

* First-class functions are like _any_ other expression,

* Can define a function, wherever you have any other expression.

| **Language** | **Syntax**                    |
|:-------------|:------------------------------|
| Haskell      | `\(x1,...,xn) -> e`           |
| Ocaml        | `fun (x1,...,xn) -> e`        |
| JS           | `(x1,...,xn) => { return e }` |
| C++          | `[&](x1,...,xn){ return e }`  |

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


### Example: Lambda Terms

We can now replace `def` as:

```python
let f    = (lambda (it): it(5))
  , incr = (lambda  (x): x + 1)
in
  f(incr)
```

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

### Implementation

As always, to the details! Lets figure out:

**Representation**

1. How to store function-tuples

**Types:**

1. Remove `Def`
2. Add `lambda` to `Expr`

**Transforms**

1. Update `tag` and `ANF`
2. Update `checker`
3. Update `compile`

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

### Implementation: Representation

Represent ``lambda-tuples'' or ``function-tuples'' via a special tag:

| Type       |   LSB |
|-----------:|------:|
| `number`   |  xx0  |
| `boolean`  |  111  |
| `pointer`  |  001  |
| `function` |  101  |

In our code:

```haskell
data Ty = ... | TClosure

typeTag :: Ty -> Arg
typeTag TTuple    = HexConst 0x00000001
typeTag TClosure  = HexConst 0x00000005

typeMask :: Ty -> Arg
typeMask TTuple   = HexConst 0x00000007
typeMask TClosure = HexConst 0x00000007
```

So, **Function Values** represented just like a tuples

* padding, `ESI` etc.
* but with tag `101`.

Crucially, we can **get** `0`-th, or `1`-st elements from tuple.

**Question:** Why not use _plain tuples_?

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



### Implementation: Types

First, lets look at the  new `Expr` type

* No more `Def`

```haskell
data Expr a
  = ...
  | Lam               [Bind a]   !(Expr a) a      -- fun. definitions
  | App     !(Expr a) [Expr a]             a      -- fun. calls
```

So we represent a **function-definition** as:

```haskell
Lam [x1,...,xn] e
```

and a **function call** as:

```haskell
App e [e1,...,en]
```

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



### Transforms: Tag

This is pretty straight forward (do it yourself)

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


### Transforms: ANF

QUIZ:

```haskell
  (App e es)
```

Does `e` need to be

* **A** Immediate  
* **B** ANF
* **C** None of the above

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


### Transforms: ANF

QUIZ:

```haskell
  (App e es)
```

Do `es` need to be

* **A** Immediate  
* **B** ANF
* **C** None of the above

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



### Transforms: ANF

The `App` case, fun + args should be **immediate**

* **Need the values to push on stack and make the call happen!**

Just like function calls (in `diamondback`), except

* Must also handle the **callee-expression** (named `e` below)

```haskell
anf i (App e es)   = (i', stitch bs (App v vs))
  where
    (i', bs, v:vs) = imms i (e:es)
```

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



### Transforms: ANF

QUIZ:

```haskell
  (Lam xs e)
```

Does `e` need to be

* **A** Immediate  
* **B** ANF
* **C** None of the above

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



### Transforms: ANF

The `Lam` case, the body will be **executed** (when called)

* So we just need to make sure its in ANF (like all the code!)

```haskell
anf i (Lam xs e) = (i', Lam xs e')
  where
    (i', e')     = anf i e
```

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


### Transforms: Checker

We just have `Expr` (no `Def`) so there is a single function:

```haskell
wellFormed :: BareExpr -> [UserError]
wellFormed = go emptyEnv
  where
    gos                       = concatMap . go
    go _    (Boolean {})      = ...
    go _    (Number  n     l) = largeNumberErrors      n l
    go vEnv (Id      x     l) = unboundVarErrors  vEnv x l
    go vEnv (Prim1 _ e     _) = ...
    go vEnv (Prim2 _ e1 e2 _) = ...
    go vEnv (If   e1 e2 e3 _) = ...
    go vEnv (Let x e1 e2   _) = ... ++ go vEnv e1 ++ go (addEnv x vEnv) e2
    go vEnv (Tuple es      _) = ...
    go vEnv (GetItem e1 e2 _) = ...
    go vEnv (App e es      _) = ?1
    go vEnv (Lam xs e      _) = ?2 ++ go ?3 e
```

* How shall we implement `?1` ?

* How shall we implement `?2` ?

* How shall we implement `?3` ?

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



### Transforms: Compiler

Finally, lets see how to convert `Expr` into `Asm`, two separate cases:

* `Lam` : definitions

* `App` : calls

### Transforms: Compiler: `Lam`

```haskell
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env (Lam xs e l) 
  = IJmp   end              -- Why?
  : ILabel start            -- Function start
  : compileDecl l xs e      -- Function code (like Decl)
 ++ ILabel end              -- Function end
  : lamTuple arity start    -- Compile fun-tuple into RAX
  where
    arity = length xs
    start = LamStart l
    end   = LamEnd   l
```

**QUESTION:** Why do we start with the `IJmp`?

```haskell
lamTuple :: Int -> Label -> [Instruction]
lamTuple arity start
  =  tupleAlloc  2                           -- alloc tuple size = 2  
  ++ tupleWrites [ repr arity                -- fill arity
                 , CodePtr start ]           -- fill code-ptr
  ++ [ IOr  (Reg RAX) (typeTag TClosure) ]   -- set the tag bits
```

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


### Transforms: Compiler: `App`

**Recall! IDEA:** Use a **tuple of** `(arity, label)`

We can now compile a call

```
  e(x1,...,xn)
```

via the following strategy:

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

1. **Evaluate** the tuple `e`
2. **Check** that `e[0]` is equal to `n` (else arity mismatch error)
3. **Call** the function at `e[1]`

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

```haskell
compileEnv env (App vE vXs)
  = assertType     env vE TClosure                    -- check vE is a function
 ++ assertArity    env vE (length vXs)                -- check vE arity
 ++ tupleReadRaw   (immArg env vE) (repr (1 :: Int))  -- load vE[1] into RAX
 ++ pushRegArgs   rArgs				      -- push reg args 
 ++ pushStackArgs sArgs				      -- push stack args
 ++ [ICall (Reg RAX)]                                 -- call RAX
 ++ popStackArgs  (length sArgs)
  where
    (rArgs, sArgs) = splitAt 6 args
```

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


## A Problem: Scope

Consider the following program:

```haskell
let one = 1
  , f   = (lambda (it): it(5))
  , inc = (lambda (n): n + one)
in
  f(inc)
```

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


## A Problem Magnified: Dynamically Created Functions

Will it work? How about this variant:

```
lambda(m): n + m        ---->   (1, label_lam_99, [n = ???])
```

```haskell
let addition = (lambda (a, b): a + b) 
let add    = (lambda (n): (lambda (m): addition(n, m)))
  , f      = (lambda (it): it(5))
  , plus1  = add(1)                 -- < 1, label_lam_99, [n := 1]>
  , plus10 = add(10)                -- < 1, label_lam_99, [n := 10]>
in
  (f(plus1), f(plus10))
```

* `add(1)` should evaluate to a **function-that-adds-1**
* `add(10)` should evaluate to a **function-that-adds-10**

Yet, its **the same code**

- same arity
- same start-label

**Problem:** How can we represent _different behaviors?_

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




## Free and Bound Variables

A variable `x` is **bound** inside an expression `e` if

- `x` is a let-bound variable inside `e`.
- `x` is a formal parameter in `e`, OR

A variable `x` is **free** inside an expression `e` if

- `x` is **not bound** inside `e`

For example consider the expression `e` :

```python
lambda (m):
  let t = m in
    n + t
```

- `m`, `t` are  **bound** inside `e`, but,
- `n` is **free** inside `e`

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


## Computing Free Variables

Lets write a function to **compute** the set of free variables.

**Question** Why *Set* ?

```
lambda(x): x + y

let x = e1 in e2

- free e1 + (free e2 - x) 
```

```
Bin Plus (Id x) (Id y)


App (Id inc) [(Id x), (Id y), (Id z)]
```

```haskell
freeVars :: Expr -> [Id]
freeVars e = S.toList (go e)
  where
    go :: Expr -> S.Set Id
    go (Id x)        = S.singleton x
    go (Number _)    = S.empty
    go (Boolean _)   = S.empty
    go (If e e1 e2)  = S.unions [go e, go e1, go e2]
    go (App e es)    = S.unions (go e : map go es) 
    go (Let x e1 e2) = S.union (go e1) (S.minus (go e2) x)
    go (Lam xs e)    = S.difference (go e) xs
```

**TODO-IN-CLASS**

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


## Free Variables and Lambdas

**Free Variables** of a `lambda`

- Those whose values come from *outside*

- Should use *the same* values whenever we "call" the `lambda`.

For example:

```haskell
let add    = (lambda (n): (lambda (m): n + m))
  , f      = (lambda (it): it(5))
  , plus1  = add(1)
  , plus10 = add(10)
in
  (f(plus1), f(plus10), plus10(20))
```

should evaluate to `(6, 15, 30)`

- `plus1` be like `lambda (m): 1  + m`
- `plus10` be like `lambda (m): 10 + m`

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

## Achieving Closure

(Recall from CSE 130)

```haskell
let add    = (lambda (n): (lambda (m): n + m))
  , f      = (lambda (it): it(5))
  , plus1  = add(1)
  , plus10 = add(10)
in
  (f(plus1), f(plus10), plus10(20))
```

should evaluate to `(6, 15, 30)`

- `plus1` be like `lambda (m): 1  + m`
- `plus10` be like `lambda (m): 10 + m`


**Key Idea:**  Each function value must **store its free variables**

represent `plus1` as:

```
(arity, code-label, [n := 1])
```

represent `plus10` as:

```
(arity, code-label, [n := 10])
```

Same code, but different free variables.

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

## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

    - **Problem:** How to map function **names** to tuples?

3. **Lambda Terms** Make functions just another expression!

    - **Problem:** How to store local variables?

4. **Function Value** `(Arity, Start-Label, Free_1, ... , Free_N)`

    - **Ta Da!**

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

## Closures: Strategy

What if we have *multiple* free variables?

```python
let foo    = (lambda (x, y):
                (lambda (z): x + y + z)
             )
  , plus10 = foo(4, 6)
  , plus20 = foo(7, 13)
in
  (plus10(0), plus20(100))
```

represent `plus10` as:

```
(arity, code-label, [x := 4], [y := 6])
```

represent `plus20` as:

```
(arity, code-label, [x := 7], [y := 13])
```

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


### Example

Lets see how to evaluate

```python
let foo    = (lambda (x, y):
                (lambda (z): x + y + z)
             )
  , plus10 = foo(4, 6)
in
  plus10(0)
```

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


### Example

Lets see how to evaluate

```python
let foo    = (lambda (x, y):
                (lambda (z): x + y + z)
             )
  , plus10 = foo(4, 6)
  , f      = (lambda (it): it(5))
in
  f(plus10)
```

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


### Implementation

**Representation**

1. How to store closures

**Types:**

- Same as before

**Transforms**

1. Update `tag` and `ANF`
    - as before

2. Update `checker`       

3. Update `compile`

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

### Representation

We can represent a **closure** as a tuple of

```
(arity, code-ptr, free-var-1, ... free-var-N)
```

which means, following the convention for tuples, as:

```
------------------------------------------------
| N + 2 | arity | code-ptr | var1 | ... | varN |
------------------------------------------------
```

Where each cell represents 64-bits / 8-bytes / 1-(double)word.

**Note:** (As with all tuples) the first word contains the #elements of the tuple.

* In this case `N + 2`

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


### Transforms: Checker

What environment should we use to check a `Lam` **body** ?

```haskell
wellFormed :: BareExpr -> [UserError]
wellFormed = go emptyEnv
  where
    ...  
    go vEnv (Lam xs e _) = errDupParams xs
                        ++ go ?vEnv e

addsEnv :: Env -> [BareBind] -> Env
addsEnv env xs = foldr addEnv env xs
```

**QUIZ** How shall we implement `?vEnv` ?

**A.** `addsEnv vEnv     []`

**B.** `addsEnv vEnv     xs`

**C.** `addsEnv emptyEnv xs`

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


### Transforms: Compile

**Question** How does the called function **know** the values of free vars?

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

- Needs to **restore them** from closure tuple

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

- Needs to **access** the closure tuple!

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


... But how shall we give the called function **access** to the tuple?

<br>
<br>
<br>
<br>
<br>
<br>
<br>


**By passing the tuple as an _extra parameter_**

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



### Transforms: Compile

**Calls** `App` (as before)


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

1. **Push** *closure-pointer* + parameters
2. **Call** code-label
3. **Pop**  *closure-pointer* + params

<br>
<br>
<br>
<br>
<br>
<br>

**Definitions** `Lam`

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

1. **Compute** *free-vars* `x1`,...,`xn`
2. **Generate** code-block
  - **Restore** free vars from closure-pointer-parameter **New**
  - **Execute** function body (as before)
3. **Allocate** tuple `(arity, code-label, x1, ... , xn)`

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

### Transforms: Compile Definitions

1. **Compute** *free-vars* `y1`,...,`yn`
2. **Generate** code-block
  - **Restore** free vars from closure-pointer-parameter
  - **Execute** function body (as before)
3. **Allocate** tuple `(arity, code-label, y1, ... , yn)`

```haskell
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env (Lam xs e l)
  = IJmp   end                       -- Why?
  : ILabel start                     -- Function start
  : lambdaBody ys xs e               -- Function code (like Decl)
 ++ ILabel end                       -- Function end
  : lamTuple arity start env ys      -- Compile closure-tuple into RAX
  where
    ys    = freeVars (Lam xs e l)
    arity = length xs
    start = LamStart l
    end   = LamEnd   l
```

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

### Creating Closure Tuples

To create the actual closure-tuple we need

<br>
<br>
<br>
<br>
<br>
<br>
<br>


* the **free-variables** `ys`
* the `env` from which to **values** of the free variables.

<br>
<br>
<br>
<br>
<br>
<br>
<br>


```haskell
lamTuple :: Int -> Label -> Env -> [Id] -> [Instruction]
lamTuple arity start env ys
  =  tupleAlloc  (2 + length ys)                    -- alloc tuple 2 + |ys|  
  ++ tupleWrites ( repr arity                       -- fill arity
                 : CodePtr start                    -- fill code-ptr
                 : [immArg env (Id y) | y <- ys] )  -- fill free-vars
  ++ [ IOr  (Reg RAX) (typeTag TClosure) ]          -- set the tag bits
```

### Generating Code Block

<br>
<br>
<br>
<br>
<br>
<br>
<br>



```haskell
lambdaBody :: [Id] -> [Id] -> AExp -> [Instruction]
lambdaBody ys xs e = 
    funEntry n               -- 1. setup  stack frame RBP/RSP
 ++ copyArgs xs'             -- 2. copy parameters to stack slots
 ++ restore nXs ys           -- 3. copy (closure) free vars to stack slots
 ++ compileEnv env body      -- 4. execute 'body' with result in RAX
 ++ funExit n                -- 5. teardown stack frame & return 
```

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

To `restore ys` we use the closure-ptr passed in at `[RDI]` 
--  the special **first** parameter -- to copy the free-vars 
`ys` onto the stack.

```haskell
restore :: Int -> [Id] -> [Instruction]
restore base ys = 
  concat [ copy i | (_, i) <- zip ys [1..]]
    where
      closV  = Reg RDI
      copy i = tupleReadRaw closV (repr (i+1))	       -- copy tuple-fld for y into RAX...
            ++ [ IMov (stackVar (base+i)) (Reg RAX) ]  -- ...write RAX into stackVar for y
```

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


## A Problem: Recursion

Oops, how do we write:

```python
def fac(n):
  if (n > 1):
    n * fac(n-1)
  else:
    1

fac(5)  
```

If we try

```python
let fac = (lambda (n):
             if (n < 1):
               1
             else:
               n * fac(n-1))
in fac(5)  
```

We get a variable unbound error!

```
Errors found!
tests/input/fac-bad.fdl:5:20-23: Unbound variable 'fac'

         5|                 n * fac(n-1))
```

We need to teach our compiler that its ok to use the name `fac` inside the body!


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
<br>
<br>

### Solution: Named Functions

We have a new form of **named functions** which looks like this:

```python
def fac(n):
  if (n < 1):
    1
  else:
    n * fac(n - 1)
in
  fac(5)
```

### Representing Named Functions

We extend `Expr` to handle such functions as:

```haskell
data Expr a
  = ...
  | Fun     (Bind a)      -- ^ name of function
            [Bind a]      -- ^ list of parameters  
            (Expr a) a    -- ^ body of function
```

Note that we parse the code

```python
def foo(x1,...,xn):
  e
in
  e'
```

as the `Expr`  

```haskell
Let foo (Fun foo [x1,...,xn] e) e'
```


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


### Compiling Named Functions

Mostly, this is left as an exercise to _you_ 

<br>
<br>
<br>
<br>
<br>

**Non-Recursive** functions

<br>
<br>
<br>
<br>
<br>

- i.e. `f` *does not* appear inside `e` in `Fun f xs e`
- Treat `Fun f xs e` as `Lam xs e` ...
- ... Everything should _just work_.

**Recursive**

<br>
<br>
<br>
<br>
<br>

- i.e. `f` *does* appear inside `e` in `Fun f xs e`
- Can you think of a simple tweak to the `Lam` strategy that works?


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recap: Functions as Values

We had functions, but they were *second-class* entities...

Now, they are *first-class* values 

* passed around as parameters
* returned from functions 
* stored in tuples etc.

How?

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

    - **Problem:** How to map function **names** to tuples?

3. **Lambda Terms** Make functions just another expression!

    - **Problem:** How to store local variables?

4. **Function Value** `(Arity, Start-Label, Free_1, ... , Free_N)`

    - **Ta Da!**


**Next:** Adding **garbage collection**

- *Reclaim!* Heap memory that is no longer in use

**Next:** Adding **static type inference**

- *Faster!* Gets rid of those annoying (and slow!) run-time checks
- *Safer!* Catches problems at compile-time, when easiest to fix!

