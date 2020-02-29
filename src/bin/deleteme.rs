//! Just for testing -- DELETE ME

use std::sync::Arc;

use dino::cir::*;

fn main() {
    let type_dint = Arc::new("DInt".to_string());
    let type_dbstr = Arc::new("DBStr".to_string());

    let struct_counter = Arc::new("structs_98e3__Counter_0f34".to_string());
    let field_count = Arc::new("count".to_string());

    let struct_game = Arc::new("structs_98e3__Game_93f2".to_string());
    let field_team_a_name = Arc::new("team_a_name".to_string());
    let field_team_a = Arc::new("team_a".to_string());
    let field_team_b_name = Arc::new("team_b_name".to_string());
    let field_team_b = Arc::new("team_b".to_string());

    let structs = vec![
        CStruct {
            name: struct_counter.clone(),
            fields: vec![
                CStructField {
                    name: field_count.clone(),
                    ptr_typ: type_dint.clone(),
                },
            ],
        },

        CStruct {
            name: struct_game.clone(),
            fields: vec![
                CStructField {
                    name: field_team_a_name.clone(),
                    ptr_typ: type_dbstr.clone(),
                },
                CStructField {
                    name: field_team_a.clone(),
                    ptr_typ: struct_counter.clone(),
                },
                CStructField {
                    name: field_team_b_name.clone(),
                    ptr_typ: type_dbstr.clone(),
                },
                CStructField {
                    name: field_team_b.clone(),
                    ptr_typ: struct_counter.clone(),
                },
            ],
        },
    ];

    let dino_out = Arc::new("__dino__out".to_string());

    let constructor_struct_counter = Arc::new("__dino__constructor__structs_98e3__Counter_0f34".to_string());

    let functions = vec![
        CFunction {
            name: constructor_struct_counter.clone(),

            params: vec![
                CFuncParam {
                    name: field_count.clone(),
                    typ: CFuncParamType::InPtr {typ: type_dint.clone()},
                },
                CFuncParam {
                    name: dino_out.clone(),
                    typ: CFuncParamType::OutPtr {typ: struct_counter.clone()},
                },
            ],

            body: vec![
                CStmt::Assign(CAssign {
                    target: CAssignTarget::OutPtr {
                        name: dino_out.clone(),
                    },
                    value: CAssignValue::Alloc {
                        typ: struct_counter.clone(),
                    },
                }),

                CStmt::Assign(CAssign {
                    target: CAssignTarget::OutPtrField {
                        name: dino_out.clone(),
                        field: field_count.clone(),
                    },
                    value: CAssignValue::Var {name: field_count.clone()},
                }),
            ],
        },
    ];

    let program = CProgram {
        structs,
        functions,
        entry_point: None, //TODO
    };
}
