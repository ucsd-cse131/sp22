---
title: Functions
date: 2021-4-22
headerImg: diamond.jpg
---

Next, we'll build **diamondback** which adds support for

* **User-Defined Functions**

In the process of doing so, we will learn about

* **Static Checking**
* **Calling Conventions**
* **Tail Recursion**

<br>
<br>
<br>
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

1. **Defining** Functions
2. **Checking** Functions
3. **Compiling** Functions
4. **Compiling** Tail Calls

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## 1. Defining Functions

First, lets add functions to our language.

As always, lets look at some examples.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Example: Increment

For example, a function that increments its input:

```python
def incr(x):
  x + 1

incr(10)
```

We have a function definition followed by a single "main"
expression, which is evaluated to yield the program's result `11`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Example: Factorial

Here's a somewhat more interesting example:

```python
def fac(n):
  let t = print(n) in
  if (n < 1):
    1
  else:
    n * fac(n - 1)

fac(5)
```

This program should produce the result

```
5
4
3
2
1
0
120
```

Suppose we modify the above to produce intermediate results:

```python
def fac(n):
  let t   = print(n)
    , res = if (n < 1):
              1
            else:
              n * fac(n - 1)
  in
    print(res)

fac(5)
```

we should now get:

```
5
4
3
2
1
0
1
1
2
6
24
120
120
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

## Example: Mutually Recursive Functions

For this language, **the function definitions are global**

> any function can call any other function.

This lets us write _mutually recursive_ functions like:

```python
def even(n):
  if (n == 0):
    true
  else:
    odd(n - 1)

def odd(n):
  if (n == 0):
    false
  else:
    even(n - 1)

let t0 = print(even(0)),
    t1 = print(even(1)),
    t2 = print(even(2)),
    t3 = print(even(3))
in
    0
```

**QUIZ** What should be the result of executing the above?

1. `false true  false true  0`
2. `true  false true  false 0`
3. `false false false false 0`
4. `true  true  true  true  0`

<br>
<br>
<br>
<br>
<br>
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

Lets add some new types to represent programs.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Bindings

Lets create a special type that represents places
where **variables are bound**,

```haskell
data Bind a = Bind Id a
```

A `Bind` is an `Id` _decorated with_ an `a` 

- to save extra _metadata_ like **tags** or **source positions**

- to make it easy to report errors.

We will use `Bind` at two places:

1. **Let**-bindings,
2. Function **parameters**.

It will be helpful to have a function to extract
the `Id` corresponding to a `Bind`

```haskell
bindId :: Bind a -> Id
bindId (Bind x _) = x
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

## Programs

A **program** is a list of declarations and _main_ expression.

```haskell
data Program a = Prog
  { pDecls :: [Decl a]    -- ^ function declarations
  , pBody  :: !(Expr a)   -- ^ "main" expression
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

## Declarations

Each **function** lives is its own **declaration**,

```haskell
data Decl a = Decl
  { fName  :: (Bind a)    -- ^ name
  , fArgs  :: [Bind a]    -- ^ parameters
  , fBody  :: (Expr a)    -- ^ body expression
  , fLabel :: a           -- ^ metadata/tag
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

## Expressions

Finally, lets add  _function application_ (calls) to the source expressions:

```haskell
data Expr a
  = ...
  | Let     (Bind a) (Expr a)  (Expr a) a
  | App     Id       [Expr a]           a
```

An _application_ or _call_ comprises

* an `Id`, the name of the function being called,
* a list of expressions corresponding to the parameters, and
* a metadata/tag value of type `a`.

(**Note:** that we are now using `Bind` instead of plain `Id` at a `Let`.)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Examples Revisited

Lets see how the examples above are represented:

```ghc
>>> parseFile "tests/input/incr.diamond"
Prog {pDecls = [Decl { fName = Bind "incr" ()
                     , fArgs = [Bind "n" ()]
                     , fBody = Prim2 Plus (Id "n" ()) (Number 1 ()) ()
                     , fLabel = ()}
               ]
     , pBody = App "incr" [Number 5 ()] ()
     }

>>> parseFile "tests/input/fac.diamond"
Prog { pDecls = [ Decl {fName = Bind "fac" ()
                , fArgs = [Bind "n" ()]
                , fBody = Let (Bind "t" ()) (Prim1 Print (Id "n" ()) ())
                          (If (Prim2 Less (Id "n" ()) (Number 1 ()) ())
                             (Number 1 ())
                             (Prim2 Times (Id "n" ())
                                (App "fac" [Prim2 Minus (Id "n" ()) (Number 1 ()) ()] ())
                                ()) ()) ()
                , fLabel = ()}
                ]
     , pBody  = App "fac" [Number 5 ()] ()
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
<br>
<br>

## 2. Static Checking

Next, we will look at an _increasingly important_ aspect
of compilation, **pointing out bugs in the code at compile time**

Called **Static Checking** because we do this _without_ (i.e. _before_)
compiling and running the code.

There is a huge spectrum of checks possible:

* Code Linting [jslint](http://jshint.com/), [hlint](https://hackage.haskell.org/package/hlint)
* Static Typing
* Static Analysis
* Contract Checking
* Dependent or [Refinement Typing](https://ucsd-progsys.github.io/liquidhaskell-blog/)

Increasingly, _this_ is the most important phase of a compiler,
and modern compiler engineering is built around making these
checks lightning fast. For more, see [this interview of Anders Hejlsberg][hejlsberg-interview]
the architect of the C# and TypeScript compilers.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Static Well-formedness Checking

We will look at code linting and, later in the quarter, type systems in 131.

For the former, suppose you tried to compile:

```python
def fac(n):
  let t = print(n) in
  if (n < 1):
    1
  else:
    n * fac(m - 1)

fact(5) + fac(3, 4)
```

We would like compilation to fail, not silently, but with useful messages:

```
$ make tests/output/err-fac.result

Errors found!

tests/input/err-fac.diamond:6:13-14: Unbound variable 'm'

         6|      n * fac(m - 1)
                         ^

tests/input/err-fac.diamond:8:1-9: Function 'fact' is not defined

         8|  fact(5) + fac(3, 4)      
             ^^^^^^^^

tests/input/err-fac.diamond:(8:11)-(9:1): Wrong arity of arguments at call of fac

         8|  fact(5) + fac(3, 4)
                       ^^^^^^^^^
```

We get _multiple_ errors:

1. The variable `m` is not defined,
1. The function `fact` is not defined,
2. The call `fac` has the wrong number of arguments.

Next, lets see how to update the architecture of our
compiler to support these and other kinds of errors.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Types: An Error Reporting API

An _error message_ type:

```haskell
data UserError = Error
  { eMsg  :: !Text          -- ^ error message
  , eSpan :: !SourceSpan    -- ^ source position
  }
  deriving (Show, Typeable)
```

We make it an _exception_ (that can be _thrown_):

```haskell
instance Exception [UserError]
```

We can **create** errors with:

```haskell
mkError :: Text -> SourceSpan -> Error
mkError msg l = Error msg l
```

We can **throw** errors with:

```haskell
abort :: UserError -> a
abort e = throw [e]
```

We **display errors** with:

```haskell
renderErrors :: [UserError] -> IO Text
```

which takes something like:

```haskell
Error
  "Unbound variable 'm'"
  { file      = "tests/input/err-fac"
  , startLine = 8
  , startCol  = 1
  , endLine   = 8
  , endCol    = 9
  }
```

and produces a **contextual message** (that requires reading the source file),

```
tests/input/err-fac.diamond:6:13-14: Unbound variable 'm'

         6|      n * fac(m - 1)
                         ^
```

We can put it all together by

```haskell
-- bin/Main.hs
main :: IO ()
main = runCompiler `catch` esHandle

esHandle :: [UserError] -> IO ()
esHandle es = renderErrors es >>= hPutStrLn stderr >> exitFailure
```

Which runs the compiler and if any `UserError` are thrown, `catch`-es and
renders the result.

<br>
<br>
<br>
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

Next, lets insert a `checker` phase into our pipeline:

![Compiler Pipeline with Checking Phase](/static/img/compiler-pipeline-functions-check.png)

In the above, we have defined the types:

```haskell
type BareP   = Program SourceSpan        -- ^ source position metadata
type AnfP    = Program SourceSpan        -- ^ sub-exprs in ANF
type AnfTagP = Program (SourceSpan, Tag) -- ^ sub-exprs have unique tag
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

## Catching Multiple Errors

Its rather irritating to get errors one-by-one.

To make using a language and compiler pleasant,
lets return _as many errors as possible_ in each run.


We will implement this by writing the functions

```haskell
wellFormed  :: BareProgram -> [UserError]
```

which will _recursively traverse_ the entire
program, declaration and expression and
return the _list of all errors_.

* If this list is empty, we just return the source unchanged,
* Otherwise, we `throw` the list of found errors (and exit.)

Thus, our `check` function looks like this:

```haskell
check :: BareProgram -> BareProgram
check p = case wellFormed p of
            [] -> p
            es -> throw es
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

## Well-formed Programs, Declarations and Expressions

The bulk of the work is done by three functions

```haskell
-- Check a whole program
wellFormed  ::                  BareProgram -> [UserError]

-- Check a single declaration
wellFormedD :: FunEnv ->        BareDecl    -> [UserError]

-- Check a single expression 
wellFormedE :: FunEnv -> Env -> Bare        -> [UserError]
```

<br>
<br>
<br>
<br>
<br>
<br>

## Well-formed Programs

To check the whole program

```haskell
wellFormed :: BareProgram -> [UserError]
wellFormed (Prog ds e)
  =  concat [wellFormedD fEnv d | d <- ds]
  ++ wellFormedE fEnv emptyEnv e
  where
    fEnv = funEnv ds

funEnv :: [Decl] -> FunEnv
funEnv ds = fromListEnv [(bindId f, length xs)
                          | Decl f xs _ _ <- ds]
```

This function,

1. **Creates** `FunEnv`, a map from _function-names_ to the _function-arity_ (number of params),
2. **Computes** the errors for each declaration (given functions in `fEnv`),
3. **Concatenates** the resulting lists of errors.

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

Which function(s) would we have to modify to
add _large number errors_ (i.e. errors for numeric literals
that may cause overflow)?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

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

Which function(s) would we have to modify to
add _variable shadowing errors_ ?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

<br>
<br>
<br>
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

Which function(s) would we have to modify to
add _duplicate parameter errors_ ?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

<br>
<br>
<br>
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

Which function(s) would we have to modify to
add _duplicate function errors_ ?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Traversals

Lets look at how we might check for two types of errors:

1. "unbound variables"
2. "undefined functions"

(In your assignment, you will look for **many** more.)

The helper function `wellFormedD` creates an _initial_
variable environment `vEnv` containing the functions
parameters, and uses that (and `fEnv`) to walk over
the body-expressions.

```haskell
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) = wellFormedE fEnv vEnv e
  where
    vEnv                         = addsEnv xs emptyEnv
```

The helper function `wellFormedE` starts with the input 

- `vEnv0` which has the function parameters, and 
- `fEnv` that has the defined functions, 

and traverses the expression:

* At each **definition** `Let x e1 e2`, the variable `x`
  is added to the environment used to check `e2`,
* At each **use** `Id x` we check if `x` is in `vEnv`
  and if not, create a suitable `UserError`
* At each **call** `App f es` we check if `f` is in `fEnv`
  and if not, create a suitable `UserError`.

```haskell
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv vEnv0 e      = go vEnv0 e
  where
    gos vEnv es               = concatMap (go vEnv) es
    go _    (Boolean {})      = []
    go _    (Number  n     l) = []
    go vEnv (Id      x     l) = unboundVarErrors vEnv x l
    go vEnv (Prim1 _ e     _) = go  vEnv e
    go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]
    go vEnv (If   e1 e2 e3 _) = gos vEnv [e1, e2, e3]
    go vEnv (Let x e1 e2   _) = go vEnv e1
                             ++ go (addEnv x vEnv) e2
    go vEnv (App f es      l) = unboundFunErrors fEnv f l
                             ++ gos vEnv es
```

You should understand the above and be able to easily add extra error checks.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## 3. Compiling Functions

![Compiler Pipeline for Functions](/static/img/compiler-pipeline-functions.png)

In the above, we have defined the types:

```haskell
type BareP   = Program SourceSpan        -- ^ each sub-expression has source position metadata
type AnfP    = Program SourceSpan        -- ^ each function body in ANF
type AnfTagP = Program (SourceSpan, Tag) -- ^ each sub-expression has unique tag
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

## Tagging

![Compiler Pipeline ANF](/static/img/compiler-pipeline-functions-tag.png)

The `tag` phase simply recursively tags each
function body and the main expression

## ANF Conversion

![Compiler Pipeline ANF](/static/img/compiler-pipeline-functions-anf.png)

* The `normalize` phase (i.e. `anf`) is recursively
  applied to each function body.

* In addition to `Prim2` operands, each call's arguments
  should be transformed into an [immediate expression](04-boa.md/#idea-immediate-expressions)

Generalize the [strategy for _binary_ operators](04-boa.md/#anf-implementation)

* from (`2` arguments) to `n`-arguments.

<br>
<br>
<br>
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

Now, lets look at _compiling_ function _definitions_ and _calls_.

![Compiler Pipeline with Checking Phase](/static/img/compiler-pipeline-functions-codegen.png)

We need a co-ordinated strategy for _definitions_ and _calls_.

**Function Definitions**

* Each _definition_ is compiled into a labeled block of `Asm`
* That implements the _body_ of the definitions.
* (But what about the _parameters_)?

**Function Calls**

* Each _call_ of `f(args)` will execute the block labeled `f`
* (But what about the _parameters_)?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Strategy: The Stack

![Stack Frames](/static/img/stack-frames-64.png)

We will use our old friend, _the stack_ to

* pass _parameters_
* have _local variables_ for called functions.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## X86-64 Calling Convention

We are using the [x86-64 calling convention](https://aaronbloomfield.github.io/pdr/book/x86-64bit-ccc-chapter.pdf), 
that ensures the following stack layout:

![Stack Layout](/static/img/stack-layout-64.png)

Suppose we have a function `foo` defined as

```python
def foo(x1,x2,...):
  e
```

When the function body **starts** executing 

- the **first 6** parameters `x1`, `x2`, ... `x6` are at
  `rdi`, `rsi`, `rdx`, `rcx`, `r8` and `r9`

- the **remaining** `x7`, `x8` ... are at 
  `[rbp + 8*2]`, `[rbp + 8*3]`, ... 

When the function **exits** 

- the **return** value is in `rax`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Pesky detail on Stack Alignment


At both _definition_ and _call_, you need to also respect the [16-Byte Stack Alignment Invariant](https://en.wikipedia.org/wiki/X86_calling_conventions)

> Ensure `rsp` is always a multiple of `16`.

i.e. pad to ensure an **even** number of arguments on stack


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Strategy: Definitions

Thus to compile each definition 

```python
def foo(x1,x2,...):
  body 
```

we must

1. **Setup Frame** to _allocate_ space for local variables by 
   ensuring that `rsp` and `rbp` are [properly managed](/lectures/05-cobra.md/#managing-the-call-stack)

2. **Copy parameters** `x1`,`x2`,... from the registers & stack  
   into stack-slots `1`,`2`,... so we can access them in the `body`

3. **Compile Body** `body` with initial `Env` mapping parameters
   `x1 => 1`, `x2 => 2`, ...

4. **Teardown Frame** to _restore_ the caller's `rbp` and `rsp` prior to `ret`urn. 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Strategy: Calls

[As before](/lectures/05-cobra.md/#in-the-caller) we must ensure
that the parameters actually live at the above address.

1. **Push** the parameter values into the registers & stack,

2. **Call** the appropriate function (using its label),

3. **Pop** the arguments off the stack by incrementing `rsp` appropriately.



<br>
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

We already have most of the machinery needed to compile calls.

Lets just add a new kind of `Label` for each user-defined function:

```haskell
data Label
  = ...
  | DefFun Id
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


## Implementation

Lets can refactor our `compile` functions into:

```haskell
-- Compile the whole program
compileProg :: AnfTagP -> Asm

-- Compile a single function declaration
compileDecl :: Bind -> [Bind] -> Expr -> Asm

-- Compile a single expression
compileExpr :: Env -> AnfTagE -> Asm
```

that respectively compile `Program`, `Decl` and `Expr`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Compiling Programs

To compile a `Program` we compile

- the *main* expression as `Decl` with no parameters and 
- each *function* declaration

```haskell
compileProg (Prog ds e) =
     compileDecl (Bind "" ()) [] e
  ++ concat [ compileDecl f xs e | (Decl f xs e _) <- ds ]
```

### QUIZ

Does it matter whether we put the code for `e` before `ds`?

**1.** Yes

**2.** No

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

Does it matter what order we compile the `ds` ?

**1.** Yes

**2.** No

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Compiling Declarations  

To compile a single `Decl` we

1. **Create a block** starting with a label for the function's name
   (so we know where to `call`),
2. Invoke `compileBody` to fill in the assembly code for the body,
   using the initial `Env` obtained from the function's formal parameters.

```haskell
compileDecl :: Bind a -> [Bind a] -> AExp -> [Instruction]
compileDecl f xs body =
 -- 0. Label for start of function
    [ ILabel (DefFun (bindId f)) ]
 -- 1. Setup  stack frame RBP/RSP
 ++ funEntry n
 -- label the 'body' for tail-calls
 ++ [ ILabel (DefFunBody (bindId f)) ]
 -- 2. Copy parameters into stack slots
 ++ copyArgs xs
 -- 3. Execute 'body' with result in RAX
 ++ compileEnv initEnv body 
 -- 4. Teardown stack frame & return
 ++ funExit n 
  where
              -- space for params + locals
    n       = length xs + countVars body
    initEnv = paramsEnv xs
```

<br>

### Setup and Tear Down Stack Frame 

(As in `cobra`)

**Setup frame**

```haskell
funEntry :: Int -> [Instruction]
funEntry n =
   [ IPush (Reg RBP)                       -- save caller's RBP
   , IMov  (Reg RBP) (Reg RSP)             -- set callee's RBP
   , ISub  (Reg RSP) (Const (argBytes n))  -- allocate n local-vars
   ]
```

**Teardown frame**

```haskell
funExit :: Int -> [Instruction]
funExit n =
   [ IAdd (Reg RSP) (Const (argBytes n))    -- un-allocate n local-vars
   , IPop (Reg RBP)                         -- restore callee's RBP 
   , IRet                                   -- return to caller
   ] 
```

<br>
<br>
<br>
<br>
<br>
<br>


### Copy Parameters into Frame

`copyArgs xs` returns the instructions needed to copy the parameter values

- **From** the combination of `rdi`, `rsi`, ... 

- **To** this function's frame, `rdi -> [rbp - 8]`, `rsi -> [rbp - 16]`,...

```haskell
copyArgs :: [a] -> Asm 
copyArgs xs    = copyRegArgs   rXs -- copy upto 6 register args
              ++ copyStackArgs sXs -- copy remaining stack args
  where
    (rXs, sXs) = splitAt 6 xs

-- Copy upto 6 args from registers into offsets 1..
copyRegArgs :: [a] -> Asm 
copyRegArgs xs = [ IMov (stackVar i) (Reg r) | (_,r,i) <- zipWith3 xs regs [1..] ]
  where regs   = [RDI, RSI, RDX, RCX, R8, R9]

-- Copy remaining args from stack into offsets 7..
copyStackArgs :: [a] -> Asm 
copyStackArgs xs = concat [ copyArg src dst | (_,src,dst) <- zip3 xs [-2,-3..] [7..] ]
  
-- Copy from RBP-offset-src to RBP-offset-dst
copyArg :: Int -> Int -> Asm
copyArg src dst = 
  [ IMov (Reg RAX) (stackVar src)
  , IMov (stackVar dst) (Reg RAX)
  ]
```

<br>
<br>
<br>
<br>
<br>
<br>

### Execute Function Body

(As in cobra) 

`compileEnv initEnv body` generates the assembly for `e` using `initEnv`, the initial `Env` created by `paramsEnv` 

```haskell
paramsEnv :: [Bind a] -> Env
paramsEnv xs = fromListEnv (zip xids [1..])
  where
    xids     = map bindId xs
```

`paramsEnv xs` returns an `Env` mapping each [parameter to its stack position](#strategy-definitions)

(Recall that `bindId` [extracts](#bindings) the `Id` from each `Bind`)

<br>
<br>
<br>
<br>
<br>
<br>




## Compiling Calls

Finally, lets extend code generation to account for calls:

```haskell
compileEnv :: Env -> AnfTagE -> [Instruction]
compileEnv env (App f vs _) = call (DefFun f) [immArg env v | v <- vs]
```

**EXERCISE** The hard work in compiling calls is done by:

```haskell
call :: Label -> [Arg] -> [Instruction]
```

which [implements the strategy for calls](#strategy-calls).
Fill in the implementation of `call` yourself. As an example,
of its behavior, consider the (source) program:

```python
def add2(x, y):
  x + y

add2(12, 7)
```

The call `add2(12, 7)` is represented as:

```haskell
App "add2" [Number 12, Number 7]
```

The code for the above call is generated by

```haskell
call (DefFun "add2") [arg 12, arg 7]
```

where `arg` [converts source values into assembly `Arg`](/lectures/05-cobra.md/a-typeclass-for-representing-constants)
which _should_ generate the equivalent of the assembly:

```nasm
  mov  rdi 24
  mov  rsi 14
  call label_def_add2
```

<br>
<br>
<br>
<br>
<br>
<br>


## 4. Compiling Tail Calls

Our language doesn't have _loops_. While recursion is more general,
it is more _expensive_ because it uses up stack space (and requires
all the attendant management overhead). For example (the `python` program):

```python
def sumTo(n):
  r = 0
  i = n
  while (0 <= i):
    r = r + i
    i = i - 1
  return r

sumTo(10000)
```

* Requires a _single_ stack frame
* Can be implemented with 2 registers

But, the "equivalent" `diamond` program

```python
def sumTo(n):
  if (n <= 0):
    0
  else:
    n + sumTo(n - 1)

sumTo(10000)
```

* Requires `10000` stack frames ...
* One for `fac(10000)`, one for `fac(9999)` etc.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Tail Recursion

Fortunately, we can do much better.

A **tail recursive** function is one where the
recursive call is the _last_ operation done by
the function, i.e. where the value returned by
the function is the _same_ as the value returned
by the recursive call.

We can rewrite `sumTo` using a tail-recursive `loop`
function:

```python
def loop(r, i):
  if (0 <= i):
    let rr = r + i
      , ii = i - 1
    in
      loop(rr, ii)   # tail call
  else:
    r

def sumTo(n):
  loop(0, n)

sumTo(10000)
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

## Visualizing Tail Calls

Lets compare the execution of the two versions of `sumTo`

### Plain Recursion

```python
sumTo(5)
==> 5 + sumTo(4)
        ^^^^^^^^
==> 5 + [4 + sumTo(3)]
             ^^^^^^^^
==> 5 + [4 + [3 + sumTo(2)]]
                  ^^^^^^^^
==> 5 + [4 + [3 + [2 + sumTo(1)]]]
                       ^^^^^^^^
==> 5 + [4 + [3 + [2 + [1 + sumTo(0)]]]]
                            ^^^^^^^^
==> 5 + [4 + [3 + [2 + [1 + 0]]]]
                        ^^^^^
==> 5 + [4 + [3 + [2 + 1]]]
                   ^^^^^
==> 5 + [4 + [3 + 3]]
              ^^^^^
==> 5 + [4 + 6]
         ^^^^^
==> 5 + 10
    ^^^^^^
==> 15
```

* Each call **pushes a frame** onto the call-stack;
* The results are **popped off** and _added_ to the parameter at that frame.

### Tail Recursion

```python
sumTo(5)
==> loop(0, 5)
==> loop(5, 4)
==> loop(9, 3)
==> loop(12, 2)
==> loop(14, 1)
==> loop(15, 0)
==> 15
```

* Accumulation happens in the parameter (not with the output),
* Each call returns its result _without further computation_

No need to use call-stack, can make recursive call **in place**.

* Tail recursive calls can be _compiled into loops_!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Tail Recursion Strategy

Here's the code for `sumTo`

## Tail Recursion Strategy

Instead of using `call` to make the call, simply:

1. **Copy** the _call's_ arguments to the (same) stack position (as current args),
  - first six in `rdi`, `rsi` etc. and rest in `[rbp+16]`, `[rbp+18]`...

2. **Jump** to the _start_ of the function
  - but _after_ the bit where setup the stack frame (to not do it again!)

That is, here's what a _naive_ implementation would look like:

```nasm
mov rdi, [rbp - 8]        # push rr
mov rsi, [rbp - 16]       # push ii
call def_loop
```

but a _tail-recursive_ call can instead be compiled as:

```nasm
mov rdi, [rbp - 8]        # push rr
mov rsi, [rbp - 16]       # push ii
jmp def_loop_body
```

which has the effect of executing `loop` _literally_ as if it were a while-loop!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Requirements

To _implement_ the above strategy, we need a way to:

1. **Identify** tail calls in the source `Expr` (AST),
2. **Compile** the tail calls following the above strategy.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Types

We can do the above in a single step, i.e., we could
identify the tail calls _during_ the code generation,
but its cleaner to separate the steps into:

![Labeling `Expr` with Tail Calls](/static/img/compiler-pipeline-tails.png)

In the above, we have defined the types:

```haskell
type BareP     = Program SourceSpan                 -- ^ each sub-expression has source position metadata
type AnfP      = Program SourceSpan                 -- ^ each function body in ANF
type AnfTagP   = Program (SourceSpan, Tag)          -- ^ each sub-expression has unique tag
type AnfTagTlP = Program ((SourceSpan, Tag), Bool)  -- ^ each call is marked as "tail" or not
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

## Transforms

Thus, to implement tail-call optimization, we need to write _two_ transforms:

**1. To Label** each call with `True` (if it is a _tail call_) or `False` otherwise:

```haskell
tails :: Program a -> Program (a, Bool)
```

**2. To Compile** tail calls, by extending `compileEnv`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Labeling Tail Calls

![Which Calls are Tail Calls?](/static/img/tail-rec-code-and-type.png)

The `Expr` in _non tail positions_

* `Prim1`
* `Prim2`
* `Let` ("bound expression")
* `If`  ("condition")

**cannot contain** tail calls; all those values have some further computation
performed on them.

However, the `Expr` in _tail positions_

* `If` ("then" and "else" branch)
* `Let` ("body")

**can contain** tail calls (_unless_ they appear under the first case)

**Algorithm:** Traverse `Expr` using a `Bool`

* Initially `True` but
* Toggled to `False` under _non-tail positions_,
* Used as "tail-label" at each call.

**NOTE:** All non-calls get a default tail-label of `False`.

```haskell
tails :: Expr a -> Expr (a, Bool)
tails = go True                                         -- initially flag is True
  where
    noTail l z             = z (l, False)
    go _ (Number n l)      = noTail l (Number n)        
    go _ (Boolean b l)     = noTail l (Boolean b)
    go _ (Id     x l)      = noTail l (Id x)

    go _ (Prim2 o e1 e2 l) = noTail l (Prim2 o e1' e2')
      where
        [e1', e2']         = go False <$> [e1, e2]      -- "prim-args" is non-tail

    go b (If c e1 e2 l)    = noTail l (If c' e1' e2')
      where
        c'                 = go False c                 -- "cond" is non-tail
        e1'                = go b     e1                -- "then" may be tail
        e2'                = go b     e2                -- "else" may be tail

    go b (Let x e1 e2 l)   = noTail l (Let x e1' e2')  
      where
        e1'                = go False e1                -- "bound-expr" is non-tail
        e2'                = go b     e2                -- "body-expr" may be tail

    go b (App f es l)      = App f es' (l, b)           -- tail-label is current flag
      where
        es'                = go False <$> es            -- "call args" are non-tail
```

**EXERCISE:**
How could we modify the above to _only_ mark **tail-recursive**
calls, i.e. to the _same_ function (whose declaration is being compiled?)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

### Compiling Tail Calls

Finally, to generate code, we need only add a special case to `compileExpr`

```haskell
compileExpr :: Env -> AnfTagTlE -> [Instruction]
compileExpr env (App f vs l)
  | isTail l  = tailcall (DefFun f)     [immArg env v | v <- vs]
  | otherwise = call     (DefFunBody f) [immArg env v | v <- vs]
```

That is, _if_ the call is _not labeled_ as a tail call,
generate code as before. Otherwise, use `tailcall` which
implements our [tail recursion strategy](#tail-recursion-strategy)

```haskell
tailcall :: Label -> [Arg] -> [Instruction]
tailcall l args
  = copyRegArgs       regArgs     -- copy into RDI, RSI,...
 ++ copyTailStackArgs stkArgs     -- copy into [RBP + 16], [RBP + 24] ...
 ++ [IJmp l]                      -- jump to start label
 where
    (regArgs, stkArgs) = splitAt 6 args
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recap

We just saw how to add support for **first-class function**

* **Definitions**, and
* **Calls**

and a way in which an important class of

> **Tail Recursive** functions can be compiled as **loops**.

Later, we'll see how to represent **functions as values** using **closures**.


[evans-x86-guide]:        http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
[mac-os-stack-alignment]: http://www.fabiensanglard.net/macosxassembly/index.php
[hejlsberg-interview]:    https://www.infoq.com/news/2016/05/anders-hejlsberg-compiler
