use std::{cell::LazyCell, collections::HashMap};

use super::{BuiltinFunction, Object};

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

    map
});
