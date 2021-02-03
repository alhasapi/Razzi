# Razzi

Razzi is an experimental compiler-interpreted for the Brainfuck language which compiles to an untyped lambda calculus written in Haskell.
According to wikipedia,_Brainfuck is an esoteric programming language..notable for its extreme minimalism, the language consists of only eight simple commands and an instruction pointer.. it is fully Turing complete_. and _Lambda calculus (also written as λ-calculus) is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution. It is a universal model of computation that can be used to simulate any Turing machine_.

## Goals: 
- *_Given the fact that BF and the lambda calculus are turing complete every program written in BF can be written in LC. From this follows that 
  there must be a program which map every LC program into BF and vice-vesa_*.
- What's interests us here is the conversion of every LC program into BF.
- Knowing that, there is already a runtime system for BF, what's remaining is just to map LC Abstract-syntax tree into BF.
- We start by enumerating the basic constructs of LC and we'll try to see how they can be encoded into BF terms:
  * Lambda-abstractions:
      - Is a way to construct higher order functions
      - multiple arguments are encoded as curried sequence of lambda-abstractions, each sharing its scope with all subsequent
        inner functions.
      - normally standard operations, like arithmetic ops, numbers, must be encoded as lambda-terms, exceptionally, as these
        constructs are native to the target language, enforcing lambda calculi's or Churchian rules on them might introduce,
        irrelevant computational overheads, which is not our aim (for now, at least!). We can consider to wrap them with an
        interface which will make them, at a language level, indistinguishable to real lambda-terms.
     -  Closures.
  * Variables lookup:
        Internally, variables either for curried functions or single argument functions map to the way they are
        sequentially arranged on the source language. Which means that, for now, the only accepted variables
        are function arguments.
  * Lambda-application:
        A lambda-application can yield either a canonical expression which maps directly to some representation into the
        target language, or it produces another lambda expression.
  * Beta-reduction:
        It is a path which leads ultimately, by applying rewriting rules, to a canonical expression      
        
## Note on The Effect Fallacy:
FP is still dwelling inside that ethereal realm of platonic ideas where mathematical structures allegedly inhabit.
In John Backus' Turing-award lecture (Can programming be liberated from the Von Neumann Style?), effects are often seen
and presented as a hideous, nearly devilish behavior. A middle path have been found finally through monads in Haskell. 
The fallacy resides in the fact that effect-free functional programs can turn at least into theorem provers, which is supposedly a good thing for a language, whithout any interaction which the "real word", making them useless.
