# Generated C Code

## Calling Convention

* All functions generated from dino code return `void`
* All arguments are pointers
* Functions that return a value take an out pointer as an argument
* Since all arguments are pointers, the out pointer is a pointer to a pointer

## Allocation

* In general, allocation is only performed in struct literal constructors
* Since many functions return new instances of a type, allocation is performed
  when creating those instances

## C Variables

In our subset of C, variables are only ever assigned to by:

* Field accesses `foo = self->bar`
* Other variables (usually temporaries) `foo = tmp342`
* Constructors from literals `foo = __dino__DInt_from_int_literal(0LL)`
* Runtime functions `foo = __dino__alloc(sizeof(Foo))`
* All variables have pointer types **except** for the condition of an `if` or
  loop which must be `bool`

## C Function Calls

* Functions may be called, but only using variables as arguments, no other
  expressions are supported
* This is to make sure that arbitrary side effects in argument expressions are
  executed in the right order

## C Conditionals

* We only use `if` and `else` because it is impossible to generate code between
  and `if` and an `else if`
* The condition may only be a simple variable

## C Statements

Within the body of a function, we only generate the following C statements:

* Uninitialized variables (for out pointers) `DBStr *foo;`
* Initialized variables using one of the forms above
* Function calls
* Conditionals

## C Structs

* All fields are pointers

We `typedef` C structs for easy access:

```c
typedef struct structs_98e3__Game_93f2 {
    DBStr *team_a_name;
    structs_98e3__Counter_0f34 *team_a;
    DBStr *team_b_name;
    structs_98e3__Counter_0f34 *team_b;
} structs_98e3__Game_93f2;
```

Notice that the name is repeated twice. This is to enable nested structures:

```c
typedef struct Tree {
    DInt *value;
    struct Tree *left;
    struct Tree *left;
} structs_98e3__Game_93f2;
```

In a nested struct, the type name must be prefixed with `struct` when declaring
the field.
