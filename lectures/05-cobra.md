---
title: Data Representation
date: 2021-4-15
headerImg: cobra.jpg
---

Next, lets add support for

* **Multiple datatypes** (`number` and `boolean`)
* **Calling** external functions

In the process of doing so, we will learn about

* **Tagged Representations**
* **Calling Conventions**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

Our plan will be to (start with `boa`) and add the following features:

1. **Representing** boolean values (and numbers)

2. **Arithmetic Operations**

3. **Arithmetic Comparisons**

4. **Dynamic Checking** (to ensure operators are well behaved)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## 1. Representation

### Motivation: Why booleans?

In the year 2021, its a bit silly to use

+ `0` for `false` and
+ non-zero for `true`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>

But really, `boolean` is a stepping stone to other data

+ Pointers
+ Tuples
+ Structures
+ Closures

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The Key Issue

How to _distinguish_ numbers from booleans?

* Need _extra_ information to mark values as `number` or `bool`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## A Word 

(A reminder for me, since I always get mixed up)

- A *Bit*  is 1-bit  
- A *Byte* is 8-bits 
- A *Word* is 2-bytes = 16 bits 
- A *Double Word* is 2-words = 4-bytes = 32 bits 
- A *Quad   Word* is 4-words = 8-bytes = 64 bits

We are working in `x86_64` where the _default_ size is a `qword`

- Registers are 64-bits
- Arithmetic is 64-bits 
- Stack slots should be 64-bits
- etc.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Option 1: Use _Two_ (Quad-)Words

How to _distinguish_ numbers from booleans?

> Need _extra_ information to mark values as `number` or `bool`.

First word is `0` means `bool`, is `1` means `number`, `2` means pointer etc.


|    Value |   Representation (HEX)        |
|---------:|------------------------------:|
|       `3`|    `[0x000000000][0x00000003]`|
|       `5`|    `[0x000000000][0x00000005]`|
|      `12`|    `[0x000000000][0x0000000c]`|
|      `42`|    `[0x000000000][0x0000002a]`|
|   `false`|    `[0x000000001][0x00000000]`|
|    `true`|    `[0x000000001][0x00000001]`|

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

**Pros**

* Can have _lots_ of different types, but

**Cons**

* Takes up _double_ memory,

* Operators `+`, `-` require _two_ memory reads.

In short, rather wasteful! We don't need **so many** types.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Option 2: Use a _Tag Bit_

Can distinguish _two_ types with a _single bit_.

*Least Significant Bit* (LSB) is

* `0` for `number`
* `1` for `boolean`

**Question**: why not `0` for `boolean` and `1` for `number`?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Tag Bit: Numbers

So `number` is the binary representation shifted left by 1 bit

* Lowest bit is always `0`
* Remaining bits are number's binary representation

For example, in binary:

|    Value |  Representation (Binary) |
|---------:|-------------------------:|
|       `3`| `[0b00000110]`           |
|       `5`| `[0b00001010]`           |
|      `12`| `[0b00011000]`           |
|      `42`| `[0b01010100]`           |

Or in hexadecimal:

|    Value |   Representation (HEX) |
|---------:|-----------------------:|
|       `3`|  `[0x06]`              |
|       `5`|  `[0x0a]`              |
|      `12`|  `[0x18]`              |
|      `42`|  `[0x54]`              |

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Tag Bit: Booleans

*Most Significant Bit* (MSB) is

* `1` for `true`
* `0` for `false`

For example

|    Value |               Representation (Binary) |
|---------:|--------------------------------------:|
|    `true`| `[0b10000000000000000000000000000001]`|
|   `false`| `[0b00000000000000000000000000000001]`|

Or, in HEX

|    Value |   Representation (HEX) |
|---------:|-----------------------:|
|    `true`|          `[0x80000001]`|
|   `false`|          `[0x00000001]`|

<br>

(eliding the 32/8 zeros in the "most-significant" `DWORD`)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Types

Lets extend our source types with `boolean` constants

```haskell  
data Expr a
  = ...
  | Boolean Bool a
```  

Correspondingly, we extend our assembly `Arg` (values) with

```haskell
data Arg
  = ...
  | HexConst  Int
```

So, our examples become:

|           Value |   Representation (HEX) |
|----------------:|-----------------------:|
|  `Boolean False`|   `HexConst 0x00000001`|
|   `Boolean True`|   `HexConst 0x80000001`|
|       `Number 3`|   `HexConst 0x00000006`|
|       `Number 5`|   `HexConst 0x0000000a`|
|      `Number 12`|   `HexConst 0x0000000c`|
|      `Number 42`|   `HexConst 0x0000002a`|

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Transforms

Next, lets update our implementation

The `parse`, `anf` and `tag` stages are straightforward.

![Compiler Pipeline](/static/img/compiler-pipeline-representation.png)


Lets focus on the `compile` function.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

#### A TypeClass for Representing Constants

Its convenient to introduce a type class describing Haskell types that can
be _represented_ as x86 arguments:

```haskell
class Repr a where
  repr :: a -> Arg
```

We can now define instances for `Int` and `Bool` as:

```haskell
instance Repr Int where
  repr n = Const (Data.Bits.shift n 1) -- left-shift `n` by 1

instance Repr Bool where
  repr False = HexConst 0x00000001
  repr True  = HexConst 0x80000001
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

#### Immediate Values to Arguments

`Boolean b` is an _immediate_ value (like `Number n`).

Lets extend `immArg` that transforms an immediate expression to an x86 argument.

```haskell
immArg :: Env -> ImmTag -> Arg
immArg (Var    x _)  = ...
immArg (Number n _)  = repr n
immArg (Boolean b _) = repr b
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

#### Compiling Constants

Finally, we can easily update the `compile` function as:

```haskell
compileEnv :: Env -> AnfTagE -> Asm
compileEnv _ e@(Number _ _)  = [IMov (Reg RAX) (immArg env e)]
compileEnv _ e@(Boolean _ _) = [IMov (Reg RAX) (immArg env e)]
```

(The other cases remain unchanged.)

Lets run some tests to double check.

<br>
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

What is the result of:

```haskell
ghci> exec "15"
```

**A.** Error

**B.** `0`

**C.** `15`

**D.** `30`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


### Output Representation

Say what?! Need to update our run-time printer in `main.c`

```c
void print(int val){
  if (val == CONST_TRUE)
    printf("true");
  else if (val == CONST_FALSE)
    printf("false");
  else // should be a number!
    printf("%d", d >> 1);  // shift right to remove tag bit.
}
```

and now we get:

```haskell
ghci> exec "15"
15
```

Can you think of some other tests we should write?

### QUIZ

What is the result of

```haskell
ghci> exec "let x = 15 in x"
```

**A.** Error

**B.** `0`

**C.** `15`

**D.** `30`

<br>
<br>
<br>
<br>
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

What is the result of

```haskell
>>> exec "if 3: 12 else: 49"
```

```haskell
>>> exec "if 0: 12 else: 49"
```

```haskell
>>> exec "if true: 12 else: 49"
```

```haskell
>>> exec "if false: 12 else: 49"
```

**A.** Error

**B.** `0`

**C.** `12`

**D.** `49`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Lets go and fix the code so the above do the right thing!

## 2. Arithmetic Operations

Constants like `2`, `29`, `false` are only useful if we can perform
computations with them.

First lets see what happens with our arithmetic operators.

### QUIZ: Addition

What will be the result of:

```haskell
ghci> exec "12 + 4"
```

**A.** Does not compile

**B.** Run-time error (e.g. segmentation fault)

**C.** `16`

**D.** `32`

**E.** `0`


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Shifted Representation and Addition

We are _representing_ a number `n` by **shifting it left by 1**

> `n` has the machine representation `2*n`

Thus, our _source values_ have the following _representations:

|    Source Value |         Representation (DEC) |
|----------------:|-----------------------------:|
|              `3`|                          `6` |
|              `5`|                         `10` |
|      `3 + 5 = 8`|                `6 + 10 = 16` |
|        `n1 + n2`|  `2*n1 + 2*n2 = 2*(n1 + n2)` |

That is, _addition_ (and similarly, _subtraction_)
works _as is_ with the shifted representation.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ: Multiplication

What will be the result (using our code so far) of:

```haskell
ghci> exec "12 * 4"
```

**A.** Does not compile

**B.** Run-time error (e.g. segmentation fault)

**C.** `24`

**D.** `48`

**E.** `96`



<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Shifted Representation and Multiplication

We are _representing_ a number `n` by **shifting it left by 1**

> `n` has the machine representation `2*n`

Thus, our _source values_ have the following _representations:

|    Source Value |         Representation (DEC) |
|----------------:|-----------------------------:|
|              `3`|                          `6` |
|              `5`|                         `10` |
|     `3 * 5 = 15`|                `6 * 10 = 60` |
|        `n1 * n2`|  `2*n1 * 2*n2 = 4*(n1 + n2)` |

<br>

Thus, multiplication ends up accumulating the factor of 2.

* Result is _two times_ the desired one.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Strategy

Thus, our strategy for compiling arithmetic operations is:

**Addition and Subtraction** "just work" 
  - as shifting "cancels out",


**Multiplication** result must be "adjusted" by dividing-by-two

  - i.e. **right shifting by 1**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Types

The _source_ language does not change at all, for the `Asm`
lets add a "right shift" instruction (`shr`):

```haskell
data Instruction
  = ...
  | IShr    Arg   Arg
```

### Transforms

We need only modify `compileEnv` to account for the "fixing up"

```haskell
compileEnv :: Env -> AnfTagE -> [Instruction]
compileEnv env (Prim2 o v1 v2 _) = compilePrim2 env o v1 v2
```

where the helper `compilePrim2` works for `Prim2` (binary) operators
and _immediate arguments_:

```haskell
compilePrim2 :: Env -> Prim2 -> ImmE -> ImmE -> [Instruction]
compilePrim2 env Plus v1 v2   = [ IMov (Reg RAX) (immArg env v1)
                                , IAdd (Reg RAX) (immArg env v2)
                                ]
compilePrim2 env Minus v1 v2  = [ IMov (Reg RAX) (immArg env v1)
                                , ISub (Reg RAX) (immArg env v2)
                                ]
compilePrim2 env Times v1 v2  = [ IMov (Reg RAX) (immArg env v1)
                                , IMul (Reg RAX) (immArg env v2)
                                , IShr (Reg RAX) (Const 1)
                                ]
```

## Tests

Lets take it out for a drive.

```haskell
ghci> exec' "2 * (0 - 1)"
4611686018427387902
```

Whoa?!

Well, its easy to figure out if you look at
the generated assembly:

```nasm
mov rax, 4
imul rax, -2
shr rax, 1
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

## Two's Complement

The **negative** result is in **twos-complement** format.

When we shift that right-by-one, we get the odd value 

- **does not "divide by two"**

| Decimal      |         Hexadecimal |
|-------------:|--------------------:|
|         `-8` | `0xFFFFFFFFFFFFFFF8`|
| `2147483644` | `0x7FFFFFFFFFFFFFFC`|

**Solution: Signed/Arithmetic Shift**

The instruction `sar`
[shift arithmetic right](https://en.wikibooks.org/wiki/X86_Assembly/Shift_and_Rotate#Arithmetic_Shift_Instructions)
does what we want, namely:

* preserves the sign-bit when shifting
* i.e. doesn't introduce a `0` by default

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Transforms Revisited

Lets add `sar` to our target:

```haskell
data Instruction
  = ...
  | ISar Arg Arg
```

and use it to fix the post-multiplication adjustment

* i.e. use `ISar` instead of `IShr`

```haskell
compilePrim2 env Times v1 v2  = [ IMov (Reg RAX) (immArg env v1)
                                , IMul (Reg RAX) (immArg env v2)
                                , ISar (Reg RAX) (Const 1)
                                ]
```

After which all is well:

```haskell
ghci> exec' "2 * (-1)"
-2
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

## 3. Arithmetic Comparisons

Next, lets try to implement comparisons:

```haskell
ghci> exec "1 < 2"
...
boa: lib/Language/Boa/Compiler.hs:(104,1)-(106,43): Non-exhaustive patterns in function compilePrim2
```

Oops. Need to implement it first!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## How to implement comparisons?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Many ways to do this:

1. branches `jne, jl, jg` or

2. bit-twiddling.

## Option 1: Comparisons via Branches

**Key Idea:** 

> Use the machine comparisons and branch

To implement `arg1 < arg2`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

```
IF 
  arg1 < arg2

THEN 
  rax := <true>

ELSE 
  rax := <false>
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

```asm
mov rax, <arg1> 
cmp rax, <arg2>       # flags are set with comparison
jg false_label        # if cmp-greater then false else true
  mov rax, <true>     # assign to RAX := true
jmp exit_label
false_label: 
  mov rax, <false>    # assign to RAX := false
exit_label:
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

## Option 2: Comparisons via Bit-Twiddling

**Key idea:**

> A *negative* number's **most significant bit** is `1`

To implement `arg1 < arg2`, compute `arg1 - arg2`

* When result is negative, MSB is `1`, ensure `rax` set to `0x80000001`
* When result is non-negative, MSB is `0`, ensure `rax` set to `0x00000001`

1. Can **extract msb** by bitwise `and` with `0x8000000000000000`.
2. Can **shift msb** to 32-position with `shr` 
2. Can **set tag bit** by bitwise ` or` with `0x00000001`

So compilation strategy is:

```nasm
mov rax, arg1
sub rax, arg2
and rax, 0x8000000000000000   ; mask out "sign" bit (msb)
shr rax, 32                   ; shift "sign" bit (msb) by 32
or  rax, 0x00000001           ; set tag bit to bool
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

### Comparisons: Implementation

Lets go and extend:

1. The `Instruction` type

```haskell
data Instruction
  = ...
  | IAnd    Arg   Arg
  | IOr     Arg   Arg
```

2. The `instrAsm` converter

```haskell
instrAsm :: Instruction -> Text
instrAsm (IAnd a1 a2) = ...
instrAsm (IOr  a1 a2) = ...
```

3. The actual `compilePrim2` function   

**Do in class**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Exercise: Comparisons via Bit-Twiddling

* Can compute `arg1 > arg2`  by computing `arg2 < arg1`.
* Can compute `arg1 != arg2` by computing `arg1 < arg2 || arg2 < arg1`
* Can compute `arg1 = arg2`  by computing `! (arg1 != arg2)`

For the above, can you figure out how to implement:

1. Boolean `!`  ?
2. Boolean `||` ?
3. Boolean `&&` ?

You may find [these instructions useful](https://en.wikibooks.org/wiki/X86_Assembly/Logic)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## 4. Dynamic Checking

We've added support for `Number` and `Boolean` but we have no way to ensure
that we don't write gibberish programs like:

```haskell
2 + true
```

or

```haskell
7 < false
```

In fact, lets try to see what happens with our code on the above:

```haskell
ghci> exec "2 + true"
```

Oops.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Static vs. Dynamic Type Checking

**Later** we will add a _static_ type system

-  that rejects meaningless programs at _compile_ time. 


**Now** lets add a _dynamic_ system 

- that _aborts execution_ with wrong _operands_ at _run_ time.

## Checking Tags at Run-Time

Here are the **allowed** types of operands for each primitive operation.

| Operation |           Op-1 |           Op-2 |
|----------:|---------------:|---------------:|
| `+`       |           `int`|           `int`|
| `-`       |           `int`|           `int`|
| `*`       |           `int`|           `int`|
| `<`       |           `int`|           `int`|
| `>`       |           `int`|           `int`|
| `&&`      |          `bool`|          `bool`|
| `||`      |          `bool`|          `bool`|
| `!`       |          `bool`|                |
| `if`      |          `bool`|                |
| `=`       | `int` or `bool`| `int` or `bool`|

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Strategy: Asserting a Type

To check if `arg` is a `number`

* Suffices to check that the LSB is `0`

* If not, jump to special `error_non_int` label

For example

```nasm
mov rax, arg
mov rbx, rax              ; copy into rbx register
and rbx, 0x00000001       ; extract lsb
cmp rbx, 0                ; check if lsb equals 0
jne error_non_number      
...
```

at `error_non_number` we can call into a `C` function:

```
error_non_number:
  mov rdi, 0              ; pass error code
  mov rsi, rax            ; pass erroneous value
  call error              ; call run-time "error" function
```

Finally, the `error` function is part of the _run-time_ and looks like:

```c
void error(long code, long v){
   if (code == 0) {
     fprintf(stderr, "Error: expected a number but got %#010x\n", v);
   }
   else if (code == 1) {
     // print out message for errorcode 1 ...
   }
   else if (code == 2) {
     // print out message for errorcode 2 ...
   } ...
   exit(1);
 }
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

## Strategy By Example

Lets implement the above in a simple file `tests/output/int-check.s`

```nasm
section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  mov rax, 1                ; not a valid number
  mov rbx, rax              ; copy into rbx register
  and rbx, 0x00000001       ; extract lsb
  cmp rbx, 0                ; check if lsb equals 0
  jne error_non_number      
error_non_number:
  mov rdi, 0
  mov rsi, rax
  call error
```

Alas

```bash
make tests/output/int-check.result
... segmentation fault ...
```

What happened ?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Managing the Call Stack

To properly call into C functions (like `error`), we must play by the rules of
the [C calling convention](https://aaronbloomfield.github.io/pdr/book/x86-64bit-ccc-chapter.pdf)

![Stack Layout](/static/img/stack-frames-64.png)

1. The _local variables_ of an (executing) function are saved in its _stack frame_.
2. The _start_ of the stack frame is saved in register `rbp`,
3. The _start_ of the _next_ frame is saved in register `rsp`.


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Calling Convention

We must **preserve the above invariant** as follows:

## In the Callee

At the **start** of the function

```nasm
push rbp          ; SAVE (previous) caller's base-pointer on stack
mov rbp, rsp      ; set our base-pointer using the current stack-pointer
sub rsp, 8*N      ; ALLOCATE space for N local variables
```

At the **end** of the function

```nasm
add rsp, 8*N0     ; FREE space for N local variables
pop rbp           ; RESTORE caller's base-pointer from stack
ret               ; return to caller
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

### Fixed Strategy By Example

Lets implement the above in a simple file `tests/output/int-check.s`

```nasm
section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push rbp                  ; save caller's base-pointer
  mov rbp, rsp              ; set our base-pointer
  sub rsp, 1600             ; alloc '100' vars

  mov rax, 1                ; not a valid number
  mov rbx, rax              ; copy into rbx register
  and rbx, 0x00000001       ; extract lsb
  cmp rbx, 0                ; check if lsb equals 0
  jne error_non_number      

  add rsp, 1600             ; de-alloc '100' vars
  pop rbp                   ; restore caller's base-pointer
  ret
error_non_number:
  mov rdi, 0
  mov rsi, rax
  call error
```

Aha, now the above works!

```bash
make tests/output/int-check.result
... expected number but got ...
```

**Q:** What NEW thing does our compiler need to compute?

**Hint:** Why do we `sub esp, 1600` above?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Types

Lets implement the above strategy.

To do so, we need a new data type for run-time types:

```haskell
data Ty = TNumber | TBoolean
```

a new `Label` for the error

```haskell
data Label
  = ...
  | TypeError Ty        -- Type Error Labels
  | Builtin   Text      -- Functions implemented in C
```

and thats it.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Transforms

The compiler must generate code to:

1. Perform dynamic type checks,
2. Exit by calling `error` if a failure occurs,
3. Manage the stack per the convention above.

### 1. Type Assertions

The key step in the implementation is to write a function

```haskell
assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty
  = [ IMov (Reg RAX) (immArg env v)
    , IMov (Reg RBX) (Reg RAX)
    , IAnd (Reg RBX) (HexConst 0x00000001)
    , ICmp (Reg RBX) (typeTag  ty)
    , IJne (TypeError ty)
    ]
```

where `typeTag` is:

```haskell
typeTag :: Ty -> Arg
typeTag TNumber  = HexConst 0x00000000
typeTag TBoolean = HexConst 0x00000001  
```

You can now splice `assertType` prior to doing the actual
computations, e.g.

```haskell
compilePrim2 :: Env -> Prim2 -> ImmE -> ImmE -> [Instruction]
compilePrim2 env Plus v1 v2   = assertType env v1 TNumber
                             ++ assertType env v2 TNumber  
                             ++ [ IMov (Reg RAX) (immArg env v1)
                                , IAdd (Reg RAX) (immArg env v2)
                                ]
```

### 2. Errors

We must also add code _at_ the `TypeError TNumber`
and `TypeError TBoolean` labels.

```haskell
errorHandler :: Ty -> Asm
errorHandler t =
  [ ILabel   (TypeError t)        -- the expected-number error
  ,   IMov   (Reg RDI) (ecode t)  -- set the first  "code" param,
  ,   IMov   (Reg RSI) (Reg RAX)  -- set the second "value" param first,
  ,   ICall  (Builtin "error")    -- call the run-time's "error" function.  
  ]

ecode :: Ty -> Arg   
ecode TNumber  = Const 0
ecode TBoolean = Const 1
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

### 3. Stack Management

**Maintaining `rsp` and `rbp`**

We need to make sure that _all_ our code respects
[the C calling convention.](#calling-convention).

To do so, just _wrap_ the generated code, with
instructions to save and restore `rbp` and `rsp`

```haskell
compileBody :: AnfTagE -> Asm
compileBody e = entryCode e
             ++ compileEnv emptyEnv e
             ++ exitCode e

entryCode :: AnfTagE -> Asm
entryCode e = [ IPush (Reg RBP)                       -- SAVE caller's RBP
              , IMov  (Reg RBP) (Reg RSP)             -- SET our RBP
              , ISub  (Reg RSP) (Const (argBytes n))  -- ALLOC n local-vars
              ]
  where
    n       = countVars e

exitCode :: AnfTagE -> Asm
exitCode e = [ IAdd (Reg RSP) (Const (argBytes n))      -- FREE n local-vars
             , IPop (Reg RBP)                           -- RESTORE caller's RBP
             , IRet                                     -- RETURN to caller
             ]
  where
    n       = countVars e
```

the `rsp` needs to be a multiple of `16` so:

```haskell
argBytes :: Int -> Int
argBytes n = 8 * n' 
  where 
    n' = if even n then n else n + 1
```

**Q:** But how shall we compute `countVars`?

Here's a shady kludge:

```haskell
countVars :: AnfTagE -> Int
countVars = 100
```

Obviously a sleazy hack (_why?_), but lets use it
to _test everything else_; then we can fix it.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## 4. Computing the Size of the Stack

Ok, now that everything (else) seems to work, lets work out:

```haskell
countVars :: AnfTagE -> Int
```

Finding the _exact_ answer is **undecidable** in general (CSE 105),
i.e. is _impossible_ to compute.

However, it is easy to find an _overapproximate_ heuristic, i.e.

* a value guaranteed to be _larger_ than the than the max size,

* and which is reasonable in practice.

As usual, lets see if we can work out a heuristic by example.

<br>
<br>
<br>
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

How many stack slots/vars are needed for the following program?

```haskell
1 + 2
```

**A.** `0`

**B.** `1`

**C.** `2`

<br>
<br>
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

How many stack slots/vars are needed for the following program?

```haskell
let x = 1
  , y = 2
  , z = 3
in
  x + y + z
```

**A.** `0`

**B.** `1`

**C.** `2`

**D.** `3`

**E.** `4`

<br>
<br>
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

How many stack slots/vars are needed for the following program?

```haskell
if true:
  let x = 1
    , y = 2
    , z = 3
  in
    x + y + z
else:
  0
```

**A.** `0`

**B.** `1`

**C.** `2`

**D.** `3`

**E.** `4`

<br>
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

How many stack slots/vars are needed for the following program?

```haskell
let x =
  let y =
    let z = 3  
    in z + 1
  in y + 1
in x + 1
```

**A.** `0`

**B.** `1`

**C.** `2`

**D.** `3`

**E.** `4`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Strategy

Let `countVars e` be:

* The _maximum_ number of let-binds in scope at any point _inside_ `e`, i.e.

* The _maximum_ size of the `Env` when compiling `e`

Lets work it out on a case-by-case basis:

* **Immediate values** like `Number` or `Var`
  * are compiled _without pushing_ anything onto the `Env`
  * i.e. `countVars` = 0

* **Binary Operations** like `Prim2 o v1 v2` take immediate values,
  * are compiled _without pushing_ anything onto the `Env`
  * i.e. `countVars` = 0

* **Branches** like `If v e1 e2` can go either way
  * can't tell at compile-time
  * i.e. worst-case is larger of `countVars e1` and `countVars e2`

* **Let-bindings** like `Let x e1 e2` require
  * evaluating `e1` and
  * _pushing_ the result onto the stack and then evaluating `e2`
  * i.e. larger of `countVars e1` and `1 + countVars e2`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Implementation

We can implement the above a simple recursive function:

```haskell
countVars :: AnfTagE -> Int  
countVars (If v e1 e2)  = max (countVars e1) (countVars e2)
countVars (Let x e1 e2) = max (countVars e1) (1 + countVars e2)
countVars _             = 0
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Naive Heuristic is Naive

The above method is quite simplistic. For example, consider the
expression:

```haskell
let x = 1
  , y = 2
  , z = 3
in
    0
```

`countVars` would tell us that we need to allocate `3` stack spaces
but clearly _none_ of the variables are actually used.

Will revisit this problem later, when looking at optimizations.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recap

We just saw how to add support for

* **Multiple datatypes** (`number` and `boolean`)
* **Calling** external functions

and in doing so, learned about

* **Tagged Representations**
* **Calling Conventions**

To get some practice, in your assignment, you will add:

1. Dynamic Checks for Arithmetic Overflows (see the `jo` and `jno` operations)
2. A Primitive `print` operation implemented by a function in the `c` run-time.

And next, we'll see how to add **user-defined functions**.
