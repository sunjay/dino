#include "./dino-runtime/target/debug/dino-runtime.h"
#include "./dino-std/target/debug/dino-std.h"

// Mangled name of struct contains a deterministic string based on its name that
// helps avoid name collisions. It also includes the mangled name of the module
// for the same purpose.
typedef struct structs_98e3__Counter_0f34 {
    // Field names aren't mangled for now since we aren't currently adding
    // fields (no need to worry about name collisions)
    DInt *count;
} structs_98e3__Counter_0f34;

// >>> declarations for all structs and forward declarations of all functions go here <<<
void structs_98e3__Counter_0f34__incr_by(structs_98e3__Counter_0f34 *self, DInt *value);

// Generated constructor -- all field values are provided as arguments
void __dino__constructor__structs_98e3__Counter_0f34(
    DInt *count,
    structs_98e3__Counter_0f34 **__dino__out
) {
    // Constructors are allowed to allocate
    *__dino__out = __dino__alloc(sizeof(structs_98e3__Counter_0f34));
    // Initializes all fields
    (*__dino__out)->count = count;
}

// All parameters are always pointers
void structs_98e3__Counter_0f34__new(structs_98e3__Counter_0f34 **__dino__out) {
    DInt *tmp203 = __dino__DInt_from_int_literal(0LL);
    __dino__constructor__structs_98e3__Counter_0f34(tmp203, __dino__out);
}

// An out pointer parameter is added when the function returns something
// Since every parameter is a pointer already, the out pointer is always a
// pointer to a pointer
// The out pointer is always the last parameter
void structs_98e3__Counter_0f34__value(structs_98e3__Counter_0f34 *self, DInt **__dino__out) {
    // All values are always split into separate variables in order to preserve
    // execution order when parameters have side-effects
    DInt *tmp453 = self->count;
    *__dino__out = tmp453;
}

// Parameter names should be mangled to avoid conflicts with temporaries
// `self` is not mangled here, but it could be in the generated code
void structs_98e3__Counter_0f34__incr(structs_98e3__Counter_0f34 *self) {
    DInt *tmp587 = __dino__DInt_from_int_literal(1LL);
    structs_98e3__Counter_0f34__incr_by(self, tmp587);
}

void structs_98e3__Counter_0f34__incr_by(structs_98e3__Counter_0f34 *self, DInt *value_a0b6) {
    // Example of field being used as an rvalue:
    DInt *tmp753 = self->count;
    DInt *tmp829 = int__add(tmp753, value_a0b6);
    // Example of field being used as an lvalue:
    self->count = tmp829;
}

void structs_98e3__Counter_0f34__decr(structs_98e3__Counter_0f34 *self) {
    DInt *tmp843 = self->count;
    DInt *tmp867 = __dino__DInt_from_int_literal(1LL);
    DInt *tmp899 = int__sub(tmp843, tmp867);
    self->count = tmp899;
}

void structs_98e3__Counter_0f34__add(
    structs_98e3__Counter_0f34 *self,
    structs_98e3__Counter_0f34 *other_f891
) {
    DInt *tmp912 = self->count;
    DInt *tmp914 = other_f891->count;
    DInt *tmp943 = int__add(tmp912, tmp914);
    self->count = tmp943;
}

typedef struct structs_98e3__Game_93f2 {
    DBStr *team_a_name;
    structs_98e3__Counter_0f34 *team_a;
    DBStr *team_b_name;
    structs_98e3__Counter_0f34 *team_b;
} structs_98e3__Game_93f2;

// Generated constructor -- all field values are provided as arguments
// Parameter names are not mangled here just because there aren't generally
// any temporaries in the auto-generated constructor. The names may be mangled
// in the actual generated code.
void __dino__constructor__structs_98e3__Game_93f2(
    DBStr *team_a_name,
    structs_98e3__Counter_0f34 *team_a,
    DBStr *team_b_name,
    structs_98e3__Counter_0f34 *team_b,
    structs_98e3__Game_93f2 **__dino__out
) {
    *__dino__out = __dino__alloc(sizeof(structs_98e3__Game_93f2));
    (*__dino__out)->team_a_name = team_a_name;
    (*__dino__out)->team_a = team_a;
    (*__dino__out)->team_b_name = team_b_name;
    (*__dino__out)->team_b = team_b;
}

void structs_98e3__Game_93f2__new(
    DBStr *team_a_name_2830,
    DBStr *team_b_name_9b75,
    structs_98e3__Game_93f2 **__dino__out
) {
    // Passing an out parameter by creating a temp variable and taking address
    structs_98e3__Counter_0f34 *tmp432;
    structs_98e3__Counter_0f34__new(&tmp432);

    structs_98e3__Counter_0f34 *tmp475;
    structs_98e3__Counter_0f34__new(&tmp475);

    // Fields passed in order does not change result since any side effects have
    // already been executed by this point. This is why we ensure that all values
    // are put into temporaries before getting passed in.
    __dino__constructor__structs_98e3__Game_93f2(team_a_name_2830, tmp432, team_b_name_9b75, tmp475, __dino__out);
}

void structs_98e3__Game_93f2__team_a_name(structs_98e3__Game_93f2 *self, DBStr **__dino__out) {
    DBStr *tmp211 = self->team_a_name;
    // This is fine because strings are immutable and we are implementing
    // rebinding semantics (like Python)
    *__dino__out = tmp211;
}

void structs_98e3__Game_93f2__team_b_name(structs_98e3__Game_93f2 *self, DBStr **__dino__out) {
    DBStr *tmp211 = self->team_b_name;
    *__dino__out = tmp211;
}

void structs_98e3__Game_93f2__team_a_scores(structs_98e3__Game_93f2 *self) {
    structs_98e3__Counter_0f34__incr(self->team_a);
}

void structs_98e3__Game_93f2__team_b_scores(structs_98e3__Game_93f2 *self) {
    structs_98e3__Counter_0f34__incr(self->team_b);
}

void structs_98e3__Game_93f2__print_winner(structs_98e3__Game_93f2 *self) {
    structs_98e3__Counter_0f34 *tmp147 = self->team_a;
    // Since this variable comes from a function/method call, we use an out variable
    DInt *tmp150;
    structs_98e3__Counter_0f34__value(tmp147, &tmp150);
    // RHS of program variables are ALWAYS generated in temporaries first, then
    // assigned later. This is something we can always optimize later if necessary.
    DInt *a_score_49e3 = tmp150;

    structs_98e3__Counter_0f34 *tmp281 = self->team_b;
    DInt *tmp161;
    structs_98e3__Counter_0f34__value(tmp281, &tmp161);
    // Program variable names are mangled to protect against conflicts with temporaries.
    DInt *b_score_8ae2 = tmp161;

    // Expressions can have arbitrary side-effects, so we still do everything in
    // temporaries and only use a single variable at the end. Structuring code
    // like this makes it impossible to use C's `else if` construct since you
    // can't declare variables inbetween the evaluation of `if` and `else if`
    DBool *tmp313 = int__gt(a_score_49e3, b_score_8ae2);
    bool tmp441 = __dino__DBool_coerce_bool(tmp313);
    if (tmp441) {
        DBStr *tmp291 = self->team_a_name;
        print_bstr(tmp291);
        DBStr *tmp292 = __dino__DBStr_from_bstr_literal("a wins", 6);
        print_bstr(tmp292);
    } else {
        DBool *tmp314 = int__lt(a_score_49e3, b_score_8ae2);
        bool tmp442 = __dino__DBool_coerce_bool(tmp314);
        if (tmp442) {
            DBStr *tmp678 = self->team_b_name;
            print_bstr(tmp678);
            DBStr *tmp688 = __dino__DBStr_from_bstr_literal("b wins", 6);
            print_bstr(tmp688);
        } else {
            DBStr *tmp777 = __dino__DBStr_from_bstr_literal("tie", 3);
            print_bstr(tmp777);
        }
    }
}

// Free function names are mangled to avoid conflicts with structs that have the
// same name. This is possible because structs and types can be declared in the
// same namespace. The mangled name contains the mangled name of the module.
// Since function names are unique within a module, this guarantees that there
// won't be any conflicts.
void structs_98e3__test_counter_889f(void) {
    DBStr *tmp112 = __dino__DBStr_from_bstr_literal("counter", 7);
    print_bstr(tmp112);
    structs_98e3__Counter_0f34 *tmp167;
    structs_98e3__Counter_0f34__new(&tmp167);
    structs_98e3__Counter_0f34 *counter_3aef = tmp167;
    DInt *tmp611;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp611);
    print_int(tmp611);

    structs_98e3__Counter_0f34__incr(counter_3aef);
    DInt *tmp612;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp612);
    print_int(tmp612);

    structs_98e3__Counter_0f34__incr(counter_3aef);
    DInt *tmp613;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp613);
    print_int(tmp613);

    structs_98e3__Counter_0f34__decr(counter_3aef);
    DInt *tmp614;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp614);
    print_int(tmp614);

    structs_98e3__Counter_0f34__decr(counter_3aef);
    DInt *tmp615;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp615);
    print_int(tmp615);

    structs_98e3__Counter_0f34__decr(counter_3aef);
    DInt *tmp616;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp616);
    print_int(tmp616);

    while (true) {
        DInt *tmp553;
        structs_98e3__Counter_0f34__value(counter_3aef, &tmp553);
        DInt *tmp412 = __dino__DInt_from_int_literal(10LL);
        DBool *tmp598 = int__lt(tmp553, tmp412);
        bool tmp572 = __dino__DBool_coerce_bool(tmp598);

        if (tmp572) {
            structs_98e3__Counter_0f34__incr(counter_3aef);

        } else {
            break;
        }
    }
    DInt *tmp617;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp617);
    print_int(tmp617);

    DInt *tmp192 = __dino__DInt_from_int_literal(34LL);
    structs_98e3__Counter_0f34__incr_by(counter_3aef, tmp192);
    DInt *tmp618;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp618);
    print_int(tmp618);

    DBStr *tmp113 = __dino__DBStr_from_bstr_literal("counter2", 8);
    print_bstr(tmp113);
    structs_98e3__Counter_0f34 *tmp811;
    structs_98e3__Counter_0f34__new(&tmp811);
    structs_98e3__Counter_0f34 *counter2_a6bc = tmp811;
    DInt *tmp742;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp742);
    DInt *tmp743 = __dino__DInt_from_int_literal(2LL);
    DInt *tmp689 = int__div(tmp742, tmp743);
    structs_98e3__Counter_0f34__incr_by(counter2_a6bc, tmp689);
    DInt *tmp619;
    structs_98e3__Counter_0f34__value(counter2_a6bc, &tmp619);
    print_int(tmp619);
    structs_98e3__Counter_0f34__add(counter2_a6bc, counter_3aef);
    DInt *tmp620;
    structs_98e3__Counter_0f34__value(counter_3aef, &tmp620);
    print_int(tmp620);
    DInt *tmp621;
    structs_98e3__Counter_0f34__value(counter2_a6bc, &tmp621);
    print_int(tmp621);
}

void structs_98e3__test_game_729e(void) {
    DBStr *tmp398 = __dino__DBStr_from_bstr_literal("game", 4);
    print_bstr(tmp398);
    DBStr *tmp487 = __dino__DBStr_from_bstr_literal("team rocket", 11);
    DBStr *tmp981 = __dino__DBStr_from_bstr_literal("team taco", 9);
    structs_98e3__Game_93f2 *tmp512;
    structs_98e3__Game_93f2__new(tmp487, tmp981, &tmp512);
    structs_98e3__Game_93f2 *game_bc3f = tmp512;

    structs_98e3__Game_93f2__team_a_scores(game_bc3f);
    structs_98e3__Game_93f2__team_b_scores(game_bc3f);
    structs_98e3__Game_93f2__team_a_scores(game_bc3f);
    structs_98e3__Game_93f2__team_a_scores(game_bc3f);

    structs_98e3__Game_93f2__print_winner(game_bc3f);
}

void structs_98e3__main_062a(void) {
    structs_98e3__test_counter_889f();
    structs_98e3__test_game_729e();
}

int main() {
    structs_98e3__main_062a();
    return 0;
}
