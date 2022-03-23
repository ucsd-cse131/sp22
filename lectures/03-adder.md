---
title: Numbers, Unary Operations, Variables
date: 2016-09-30
headerImg: adder.jpg
---

# Lets Write a Compiler!

Our goal is to write a compiler which is a function:

```haskell
compiler :: SourceProgram -> TargetProgram
```

In 131 `TargetProgram` is going to be a binary executable.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lets write our first Compilers

`SourceProgram` will be a  sequence of four _tiny_ "languages"

1. Numbers
  * e.g. `7`, `12`, `42` ...

2. Numbers + Increment
  * e.g. `add1(7)`, `add1(add1(12))`, ...

3. Numbers + Increment + Decrement
  * e.g. `add1(7)`, `add1(add1(12))`, `sub1(add1(42))`

4. Numbers + Increment + Decrement + Local Variables
  * e.g. `let x = add1(7), y = add1(x) in add1(y)`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recall: What does a Compiler *look like*?

![Compiler Pipeline](/static/img/compiler-pipeline.png)

An input source program is converted to an executable binary in many stages:

* **Parsed** into a data structure called an **Abstract Syntax Tree**
* **Checked** to make sure code is well-formed (and well-typed)
* **Simplified** into some convenient **Intermediate Representation**
* **Optimized** into (equivalent) but faster program
* **Generated** into assembly `x86`
* **Linked** against a run-time (usually written in C)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Simplified Pipeline

**Goal:** Compile _source_ into _executable_ that,
when run, **prints** the result of evaluating the source.

**Approach:** Lets figure out how to write

1. A **compiler** from the input _string_ into _assembly_,
2. A **run-time** that will let us do the printing.

![Simplified Compiler Pipeline with Runtime](/static/img/compiler-pipeline-1-2.png)

Next, lets see how to do (1) and (2) using our
sequence of `adder` languages.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

# Adder-1

1. Numbers
  * e.g. `7`, `12`, `42` ...


## The "Run-time"

Lets work _backwards_ and start with the run-time.

Here's what it looks like as a `C` program `main.c`

```c
#include <stdio.h>

extern int our_code() asm("our_code_label");

int main(int argc, char** argv) {
  int result = our_code();
  printf("%d\n", result);
  return 0;
}
```

* `main` just calls `our_code` and prints its return value,
* `our_code` is (to be) implemented in assembly,
  * Starting at **label** `our_code_label`,
  * With the desired _return_ value stored in register `EAX`
  * per, the `C` [calling convention][x86-64-guide]  

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Test Systems in Isolation

**Key idea in (Software) Engineering:**

> Decouple systems so you can test one component
> without (even implementing) another.

Lets test our "run-time" without even building the compiler.

### Testing the Runtime: A Really Simple Example

Given a `SourceProgram`

```
42
```

We _want to_ compile the above into an assembly
file `forty_two.s` that looks like:

```asm
section .text
global our_code_label
our_code_label:
  mov eax, 42
  ret
```  

For now, lets just

* _write_ that file by hand, and test to ensure
* _object-generation_ and then
* _linking_ works

(On MacOS)

```bash
$ nasm -f macho64 -o forty_two.o forty_two.s
$ clang -g -m64 -o forty_two.run c-bits/main.c forty_two.o
```

(On Linux)

```bash
$ nasm -f elf64 -o forty_two.o forty_two.s
$ clang -g -m64 -o forty_two.run c-bits/main.c forty_two.o
```

We can now run it:

```bash
$ forty_two.run
42
```

Hooray!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The "Compiler"


Recall, that compilers were invented to [avoid writing assembly by hand](01-introduction.md/#a-bit-of-history)

## First Step: Types

To go from source to assembly, we must do:

![Simplified Pipeline](/static/img/simplified-pipeline-short.png)

Our first step will be to **model** the problem domain using **types**.

![Simplified Pipeline with Types](/static/img/simplified-pipeline-short-types.png)

Lets create types that represent each intermediate value:

* `Text` for the raw input source
* `Expr` for the AST
* `Asm`  for the output x86 assembly

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Defining the Types: `Text`

`Text` is raw strings, i.e. sequences of characters

```haskell
texts :: [Text]
texts =  
  [ "It was a dark and stormy night..."
  , "I wanna hold your hand..."
  , "12"
  ]
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

## Defining the Types: `Expr`

We convert the `Text` into a tree-structure defined by the datatype

```haskell
data Expr = Number Int
```

**Note:** As we add features to our language, we will keep adding cases to `Expr`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Defining the Types: `Asm`

Lets also do this _gradually_ as [the x86 instruction set is HUGE!][x86-full]

Recall, we need to represent

```asm
section .text
global our_code_label
our_code_label:
  mov eax, 42
  ret
```  

An `Asm` program is a **list of instructions** each of which can:

* Create a `Label`, or
* Move a `Arg` into a `Register`
* `Return` back to the run-time.

```haskell
type Asm = [Instruction]

data Instruction
  = ILabel Text
  | IMov   Arg Arg
  | IRet
```

Where we have

```haskell
data Register
  = EAX

data Arg
  = Const Int       -- a fixed number
  | Reg   Register  -- a register
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

## Second Step: Transforms

Ok, now we just need to write the functions:

```haskell
parse   :: Text -> Expr     -- 1. Transform source-string into AST
compile :: Expr -> Asm      -- 2. Transform AST into assembly
asm     :: Asm  -> Text     -- 3. Transform assembly into output-string
```

Pretty straightforward:

```haskell
parse :: Text -> Expr
parse    = parseWith expr
  where
    expr = integer

compile :: Expr -> Asm
compile (Number n) =
  [ IMov (Reg EAX) (Const n)
  , IRet
  ]

asm :: Asm -> Text
asm is = L.intercalate "\n" [instr i | i <- is]
```

Where `instr` is a `Text` representation of _each_ `Instruction`

```haskell
instr :: Instruction -> Text
instr (IMov a1 a2) = printf "mov %s, %s" (arg a1) (arg a2)

arg :: Arg -> Text
arg (Const n) = printf "%d" n
arg (Reg r)   = reg r

reg :: Register -> Text
reg EAX = "eax"
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

### Brief digression: Typeclasses

Note that above we have _four_ separate functions that crunch
different types to the `Text` representation of x86 assembly:

```haskell
asm   :: Asm -> Text
instr :: Instruction -> Text
arg   :: Arg -> Text
reg   :: Register -> Text
```

Remembering names is _hard_.

We can write an **overloaded** function, and let the compiler figure
out the correct implementation from the type, using **Typeclasses**.

The following defines an _interface_ for all those types `a` that
can be converted to x86 assembly:

```
class ToX86 a where
  asm :: a -> Text
```

Now, to overload, we say that each of the types
`Asm`, `Instruction`, `Arg` and `Register`
_implements_ or **has an instance of** `ToX86`

```haskell
instance ToX86 Asm where
  asm is = L.intercalate "\n" [asm i | i <- is]

instance ToX86 Instruction where
  asm (IMov a1 a2) = printf "mov %s, %s" (asm a1) (asm a2)

instance ToX86 Arg where
  asm (Const n) = printf "%d" n
  asm (Reg r)   = asm r

instance ToX86 Register where
  asm EAX = "eax"
```

**Note** in each case above, the compiler figures out the _correct_ implementation,
from the types...

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

# Adder-2

Well that was easy! Lets beef up the language!

2. Numbers + Increment
  * e.g. `add1(7)`, `add1(add1(12))`, ...

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Repeat our Recipe

1. Build intuition with **examples**,
2. Model problem with **types**,
3. Implement compiler via **type-transforming-functions**,
4. Validate compiler via **tests**.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## 1. Examples

First, lets look at some examples.

## Example 1

How should we compile?

```
add1(7)
```

In English

1. Move `7` into the `eax` register
2. Add  `1` to the contents of `eax`

In ASM

```asm
mov eax, 7
add eax, 1    
```

Aha, note that `add` is a new kind of `Instruction`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



## Example 2

How should we compile

```
add1(add1(12))
```

In English

1. Move `12` into the `eax` register
2. Add  `1`  to the contents of `eax`
3. Add  `1`  to the contents of `eax`

In ASM

```asm
mov eax, 12
add eax, 1
add eax, 1
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




## Compositional Code Generation

Note correspondence between sub-expressions of _source_ and _assembly_

![Compositional Compilation](/static/img/compositional-compilation.png)

We will write compiler in **compositional** manner

* Generating `Asm` for each _sub-expression_ (AST subtree) independently,
* Generating `Asm` for _super-expression_, assuming the value of sub-expression is in `EAX`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>




## 2. Types

Next, lets extend the types to incorporate new language features

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


### Extend Type for Source and Assembly

**Source Expressions**

```haskell
data Expr = ...
          | Add1   Expr
```

**Assembly Instructions**

```haskell
data Instruction
  = ...
  | IAdd  Arg Arg
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


### Example-1 Revisited

```haskell
src1 = "add1(7)"

exp1 = Add1 (Number 7)

asm1 = [ IMov (Reg EAX) (Const 7)
       , IAdd (Reg EAX) (Const 1)
       ]
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

### Example-2 Revisited

```haskell
src2 = "add1(add1(12))"

exp2 = Add1 (Add1 (Number 12))

asm2 = [ IMov (Reg EAX) (Const 12)
       , IAdd (Reg EAX) (Const 1)
       , IAdd (Reg EAX) (Const 1)
       ]
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

## 3. Transforms

Now lets go back and suitably extend the transforms:

```haskell
parse   :: Text -> Expr     -- 1. Transform source-string into AST
compile :: Expr -> Asm      -- 2. Transform AST into assembly
asm     :: Asm  -> Text     -- 3. Transform assembly into output-string
```

Lets do the easy bits first, namely `parse` and `asm`

### Parse

```haskell
parse :: Text -> Expr
parse = parseWith expr

expr :: Parser Expr
expr =  try primExpr
    <|> integer  

primExpr :: Parser Expr
primExpr = Add1 <$> rWord "add1" *> parens expr
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

### Asm

To update `asm` just need to handle case for `IAdd`

```haskell
instance ToX86 Instruction where
  asm (IMov a1 a2) = printf "mov %s, %s" (asm a1) (asm a2)
  asm (IAdd a1 a2) = printf "add %s, %s" (asm a1) (asm a2)
```

**Note**

1. GHC will _tell_ you exactly which functions need to be extended (Types, FTW!)
2. We will not discuss `parse` and `asm` any more...

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Compile

Finally, the key step is

```haskell
compile :: Expr -> Asm
compile (Number n)
  = [ IMov (Reg EAX) (Const n)
    , IRet
    ]
compile (Add1 e)
  = compile e                    -- EAX holds value of result of `e` ...
 ++ [ IAdd (Reg EAX) (Const 1) ] -- ... so just increment it.
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

### Examples Revisited

Lets check that compile behaves as desired:

```haskell
>>> (compile (Number 12)
[ IMov (Reg EAX) (Const 12) ]

>>> compile (Add1 (Number 12))
[ IMov (Reg EAX) (Const 12)
, IAdd (Reg EAX) (Const 1)
]

>>> compile (Add1 (Add1 (Number 12)))
[ IMov (Reg EAX) (Const 12)
, IAdd (Reg EAX) (Const 1)
, IAdd (Reg EAX) (Const 1)
]
```

# Adder-3

You do it!

3. Numbers + Increment + Double
  * e.g. `add1(7)`, `twice(add1(12))`, `twice(twice(add1(42)))`

# Adder-4

4. Numbers + Increment + Decrement + Local Variables
  * e.g. `let x = add1(7), y = add1(x) in add1(y)`

Can you think why **local variables** make things more interesting?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Repeat our Recipe

1. Build intuition with **examples**,
2. Model problem with **types**,
3. Implement compiler via **type-transforming-functions**,
4. Validate compiler via **tests**.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Step 1: Examples

Lets look at some examples

### Example: let1

```haskell
let x = 10
in
    x
```

Need to store 1 variable -- `x`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Example: let2

```haskell
let x = 10         -- x = 10
  , y = add1(x)    -- y = 11
  , z = add1(y)    -- z = 12
in
    add1(z)        -- 13
```

Need to store 3 variables-- `x, y, z`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Example: let3

```haskell
let a = 10
  , c = let b = add1(a)
        in
           add1(b)
in
    add1(c)
```

Need to store 3 variables -- `a`, `b`, `c` -- but **at most 2 at a time**

* First `a, b`, then `a, c`
* Don't need `b` and `c` simultaneously

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Problem: Registers are Not Enough

A single register `eax` is useless:

  * May need 2 or 3 or 4 or 5 ... values.

There is only a _fixed_ number (say, `N`) of registers

  * And our programs may need to store more than `N` values, so

Need to dig for more storage space!

### Memory: Code, Globals, Heap and Stack

Here's what the memory -- i.e. storage -- looks like:

![Memory Layout](/static/img/memory-layout.png)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Focusing on "The Stack"

Lets zoom into the stack region, which when we start looks like this:

![Stack Layout](/static/img/stack-layout.png)

The stack **grows downward** (i.e. to **smaller** addresses)

We have *lots* of 4-byte slots on the stack at offsets from the "stack pointer" at addresses:

* `[RBP - 4 * 1]`, `[RBP - 4 * 2]`, `[RBP - 4 * 3]` ...,

**Note:** On 32-bit machines the "base" is the `EBP` register (not `RBP`).

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



### How to compute mapping from _variables_ to _slots_ ?

The `i`-th _stack-variable_ lives at address `[RBP - 4 * i]`

**Required** A mapping

* From _source variables_ (`x`, `y`, `z` ...)
* To _stack positions_ (`1`, `2`, `3` ...)

**Solution** The structure of the `let`s is stack-like too...

* Maintain an `Env` that maps `Id |-> StackPosition`

`let x = e1 in e2` adds `x |-> i` to `Env`

* where `i` is ``current'' size of stack.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Let-bindings and Stacks: Example-1

```haskell
                    -- []
let x = 1 
in                  -- [ x |-> 1 ]
    x
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



### Let-bindings and Stacks: Example-2

```haskell
                          -- []
let x = 1           
                          -- [x |-> 1]
  , y = add1(x)     
                          -- [y |-> 2, x |-> 1]
  , z = add1(y)     
in                        -- [z |- 3, y |-> 2, x |-> 1]
    add1(z)              
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


### QUIZ

At what position on the stack do we store variable `c` ?

```haskell
let a = 1
  , c =
        let b = add1(a)
        in add1(b)
in
    add1(c)
```

**A.** 1

**B.** 2

**C.** 3

**D.** 4

**E.** not on stack!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


### Strategy


```haskell
              -- ENV(n)
let x = E1 
in            -- [x |-> n+1, ENV(n)]
   E2 
              -- ENV(n)
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




### Strategy: Variable Definition

At each point, we have `env` that maps (previously defined) `Id` to `StackPosition`

To compile `let x = e1 in e2` we

1. Compile `e1` using `env` (i.e. resulting value will be stored in `eax`)
2. Move `eax` into `[RBP - 4 * i]`
3. Compile `e2` using `env'`

(where `env'` be `env` with `x |-> i` i.e. push `x` onto `env` at position `i`)


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Strategy: Variable Use

To compile `x` given `env`

1. Move `[RBP - 4 * i]` into `eax`

(where `env` maps `x |-> i`)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


### Example: Let-bindings to `Asm`

Lets see how our strategy works by example:

### Example: let1

![Convert let1 to Assembly](/static/img/let-1-to-asm.png)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### QUIZ: let2

When we compile

```haskell 
let x = 10 
  , y = add1(x)
in
  add1(y)
```

The assembly looks like

```asm
mov eax, 10                ; LHS of let x = 10
mov [RBP - 4*1], eax       ; save x on the stack 
mov eax, [RBP - 4*1]       ; LHS of   , y = add1(x) 
add eax, 1                 ; "" 
???
add eax, 1
```

What .asm instructions shall we fill in for `???` 

```asm
mov [RBP - 4 * 1], eax    ; A  
mov eax, [RBP - 4 * 1]

mov [RBP - 4 * 1], eax    ; B  

mov [RBP - 4 * 2], eax    ; C  

mov [RBP - 4 * 2], eax    ; D  
mov eax, [RBP - 4 * 2]

                          ; E  (empty! no instructions)
```

<!-- ![Convert let2 to Assembly](/static/img/let-2-to-asm.png) -->

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Example: let3

Lets compile

```haskell 
let a = 10 
  , c = let b = add1(a)
        in 
            add1(b)
in
    add1(c)
```

Lets figure out what the assembly looks like!

```asm
mov eax, 10                ; LHS of let a = 10
mov [RBP - 4*1], eax       ; save a on the stack 
??? 
```
<!-- ![Convert let3 to Assembly](/static/img/let-3-to-asm.png) --> 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Step 2: Types

Now, we're ready to move to the implementation!

**Source Expressions**

```haskell
type Id   = Text

data Expr = ...
          | Let Id Expr Expr    -- `let x = e1 in e2` represented as `Let x e1 e2`
          | Var Id              -- `x` represented as `Var x` 
```

**Assembly Instructions**

Lets enrich the `Instruction` to include the register-offset `[RBP - 4*i]`

```haskell
data Arg = ...
         | RegOffset Reg Int    -- `[RBP - 4*i]` modeled as `RegOffset RBP i`
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

### Environments

An `Env` type to track *stack-positions* of variables with **API**

* `push` variable onto `Env` (returning its position),
* `lookup` a variable's position in `Env`

```haskell
push :: Id -> Env -> (Int, Env)
push x env = (i, (x, i) : env)
  where
    i      = 1 + length env

lookup :: Id -> Env -> Maybe Int
lookup x ((y, i) : env)
  | x == y              = Just i
  | otherwise           = lookup x env   
lookup x []             = Nothing
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

## Step 3: Transforms

Almost done: just write code formalizing the [above strategy](#strategy)

### Code: Variable Use

```haskell
compileEnv env (Var x) = [ IMov (Reg EAX) (RegOffset RBP i) ]
  where
    i                  = fromMaybe err (lookup x env)
    err                = error (printf "Error: Variable '%s' is unbound" x)
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

### Code: Variable Definition

```haskell
compileEnv env (Let x e1 e2 l)  = compileEnv env  e1  
                               ++ IMov (RegOffset RBP i) (Reg EAX)
                                : compileEnv env' e2
      where
        (i, env')               = pushEnv x env
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

## Step 4: Tests

Lets take our `adder` compiler out for a spin!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recap: We just wrote our first Compilers

`SourceProgram` will be a  sequence of four _tiny_ "languages"

1. Numbers
  * e.g. `7`, `12`, `42` ...

2. Numbers + Increment
  * e.g. `add1(7)`, `add1(add1(12))`, ...

3. Numbers + Increment + Decrement
  * e.g. `add1(7)`, `add1(add1(12))`, `sub1(add1(42))`

4. Numbers + Increment + Decrement + Local Variables
  * e.g. `let x = add1(7), y = add1(x) in add1(y)`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Using a Recipe

1. Build intuition with **examples**,
2. Model problem with **types**,
3. Implement compiler via **type-transforming-functions**,
4. Validate compiler via **tests**.

Will iterate on this till we have a pretty kick-ass language.

[x86-64-guide]: https://web.stanford.edu/class/cs107/guide/x86-64.html
[evans-x86-guide]: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
[x86-full]: http://www.felixcloutier.com/x86/
