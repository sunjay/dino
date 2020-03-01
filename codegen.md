# Generated C Code

## Calling Convention

* All functions generated from dino code return `void`
* All arguments are pointers
* Functions that return a value take an out pointer as an argument
* Since all arguments are pointers, the out pointer is a pointer to a pointer
* Using out pointers rather than return values is useful for extending the
  convention later to support multiple output parameters

## Allocation

* In general, allocation is only performed in struct literal constructors
* Since many functions return new instances of a type, allocation is performed
  when creating those instances
* A consequence of this is that as long as the generated constructor for each
  struct performs allocation, the generated code never needs to generate
  allocations in variable decls, etc.

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

## C Loops

* We only use infinite `while (true)` loops because evaluating the conditional
  might require multiple statements
* The loop follows the following general structure:

```c
while (true) {
    // ...code to evaluate the condition...
    bool tmp123 = ...;

    if (tmp123) {
        // ...code for loop body...

    } else {
        // Stop the loop once the condition is no longer satisfied
        break;
    }
}
```

## C Statements

Within the body of a function, we only generate the following C statements:

* Uninitialized variables (for out pointers) `DBStr *foo;`
* Initialized variables using one of the forms above
* Function calls
* Early return `return;`
* Conditionals
* Loops
* Loop control flow `break;`

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

## Notes

* list of structs and their mangled names
* for each struct, a list of each field **in order** with:
  * field (mangled) name
  * field type mangled name
* mangled name of struct literal constructor
* list of methods and their mangled names
* for each method:
  * list of parameters and the mangled names of their types
