KOAK
====

This is a compiler for the KOAK (Kind Of Advanced Kaleidoscope) language.  
It is a little statically-typed language, to which we made extensions (feature-wise and syntax-wise).  

Features
--------

Language features include:
- Traits (also called type-classes)
- Parametric polymorphism
- Function monomorphisation (also called specialization or instantiation)
- First-class functions
- Anonymous functions (also called lambdas)
- User-defined binary and unary operators (with custom precedence and associativity)
- Overloaded literals
- FFI capability with C functions

Sample code
-----------

Custom operators:
```
def binary ** 60 right (x:integer y:integer): integer
    (ret = 1) :
    (for i = 0, i < y, i = i + 1 in
        ret = ret * x);

5 ** 10; -- 1024
```

Extern functions (C FFI):
```
-- Declare C standard library function
extern putchar(c:integer): integer;

for i = 65, i < 91, i = i + 1 in putchar(i); -- Capital letters
for i = 97, i < 123, i = i + 1 in putchar(i); -- Non-capital letters
```

Lambdas and custom operators:
```
-- Left to right function application
def binary |> 5 left (x:integer f:(integer) -> integer): integer
    f(x);

-- Right to left function application
def binary <| 5 right (f:(integer) -> integer x:integer): integer
    f(x);

5 |> (x -> x + 2) |> (x -> x * 2); -- 14
(x -> x * 2) <| (x -> x + 2) <| 5; -- 14
```

Traits:
```
5 + 2;          -- Compiles fine as `integer` implements the `Num` trait.
4.0 + 3.0;      -- Also compiles fine as `double` also implements the `Num` trait.
() + ();        -- Does not compile, `void` does not implement the `Num` trait.
true + false;   -- Does not compile, `bool` also does not implement the `Num` trait.

extern putchar(c:integer): integer;

-- Prints a digit to the screen
def putdigit(d:integer): integer
    putchar(d + 48);

-- As `integer` implements the `Default` trait,
-- we can use `default()` to get a default value of this type.
-- For integers, it is just 0.
i = default();

putdigit(i); -- Prints 0
```
