use std::{cell::LazyCell, collections::HashMap};

use super::{Array, BuiltinFunction, Object};

pub const BUILTINS: LazyCell<HashMap<&'static str, Object>> = LazyCell::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "len",
        Object::Builtin(BuiltinFunction {
            func: Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::String(s) => Object::Integer(s.len() as i64),
                    Object::Array(s) => Object::Integer(s.elements.len() as i64),
                    v => Object::Error(format!("argument to `len` not supported, got {}", v)),
                }
            }),
        }),
    );

    map.insert(
        "first",
        Object::Builtin(BuiltinFunction {
            func: Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::Array(s) => s.elements[0].clone(),
                    v => Object::Error(format!("argument to `len` not supported, got {}", v)),
                }
            }),
        }),
    );

    map.insert(
        "last",
        Object::Builtin(BuiltinFunction {
            func: Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::Array(s) => {
                        let last = s.elements.len();
                        s.elements[last - 1].clone()
                    }
                    v => Object::Error(format!("argument to `len` not supported, got {}", v)),
                }
            }),
        }),
    );

    map.insert(
        "rest",
        Object::Builtin(BuiltinFunction {
            func: Box::new(|args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::Array(s) => Object::Array(Array {
                        elements: s.elements.iter().skip(1).cloned().collect(),
                    }),
                    v => Object::Error(format!("argument to `len` not supported, got {}", v)),
                }
            }),
        }),
    );

    map.insert(
        "push",
        Object::Builtin(BuiltinFunction {
            func: Box::new(|args: Vec<Object>| {
                if args.len() != 2 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}, want=2",
                        args.len()
                    ));
                }

                match &args[0] {
                    Object::Array(s) => {
                        let mut elements = s.elements.clone();
                        elements.push(args[1].clone());

                        Object::Array(Array { elements })
                    }
                    v => Object::Error(format!("argument to `len` not supported, got {}", v)),
                }
            }),
        }),
    );
    map
});
