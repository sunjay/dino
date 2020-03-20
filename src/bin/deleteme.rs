//! Just for testing -- DELETE ME

use termcolor::ColorChoice;
use dino::parser::parse_module;
use dino::diagnostics::Diagnostics;
use dino::cprintln;
use dino::cir::CSymbols;
use dino::cgenir::*;

fn main() {
    let diag = Diagnostics::new(ColorChoice::Always);
    parse_module(include_str!("../../tests/run-pass/merge-sort.dino"), &diag);

    let mut syms = CSymbols::default();

    let type_dint = syms.insert_overwrite("DInt".to_string());
    let type_dbstr = syms.insert_overwrite("DBStr".to_string());

    let struct_counter = syms.insert_overwrite("structs_98e3__Counter_0f34".to_string());
    let field_count = syms.insert_overwrite("count".to_string());

    let struct_game = syms.insert_overwrite("structs_98e3__Game_93f2".to_string());
    let field_team_a_name = syms.insert_overwrite("team_a_name".to_string());
    let field_team_a = syms.insert_overwrite("team_a".to_string());
    let field_team_b_name = syms.insert_overwrite("team_b_name".to_string());
    let field_team_b = syms.insert_overwrite("team_b".to_string());

    let structs = vec![
        Struct {
            name: struct_counter.clone(),
            fields: vec![
                StructField {
                    name: field_count.clone(),
                    ptr_typ: type_dint.clone(),
                },
            ],
        },

        Struct {
            name: struct_game.clone(),
            fields: vec![
                StructField {
                    name: field_team_a_name.clone(),
                    ptr_typ: type_dbstr.clone(),
                },
                StructField {
                    name: field_team_a.clone(),
                    ptr_typ: struct_counter.clone(),
                },
                StructField {
                    name: field_team_b_name.clone(),
                    ptr_typ: type_dbstr.clone(),
                },
                StructField {
                    name: field_team_b.clone(),
                    ptr_typ: struct_counter.clone(),
                },
            ],
        },
    ];

    let dino_out = syms.insert_overwrite("__dino__out".to_string());

    let constructor_struct_counter = syms.insert_overwrite("__dino__constructor__structs_98e3__Counter_0f34".to_string());

    let functions = vec![
        Function {
            name: constructor_struct_counter.clone(),

            in_params: vec![
                InParam {
                    name: field_count.clone(),
                    ptr_typ: type_dint.clone(),
                },
            ],
            out_param: OutParam {
                name: dino_out.clone(),
                ptr_typ: struct_counter.clone(),
            },

            body: vec![
                Stmt::Assign(Assign {
                    target: AssignTarget::OutPtr {
                        name: dino_out.clone(),
                    },
                    value: AssignValue::Alloc {
                        ty: struct_counter.clone(),
                    },
                }),

                Stmt::Assign(Assign {
                    target: AssignTarget::OutPtrField {
                        name: dino_out.clone(),
                        field: field_count.clone(),
                    },
                    value: AssignValue::Var {name: field_count.clone()},
                }),
            ],
        },
    ];

    let program = Program {
        structs,
        functions,
        entry_point: None, //TODO
    };

    cprintln!(&syms, "{}", program.to_c());
}
