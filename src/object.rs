use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
    ops::{Add, Div, Mul, Sub},
};

use crate::{ast::Statement, code, compiler, token::IdentToken};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Object {
    Null,
    Int(IntObject),
    Bool(BoolObject),
    Return(ReturnObject),
    String(StringObject),
    Fn(FnObject),
    Array(ArrayObject),
    Hash(HashObject),
    CompiledFn(CompiledFnObject),
}

impl Object {
    pub fn add(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs + rhs).into()),
            _ => None,
        }
    }

    pub fn sub(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs - rhs).into()),
            _ => None,
        }
    }

    pub fn mul(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs * rhs).into()),
            _ => None,
        }
    }

    pub fn div(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some((lhs / rhs).into()),
            _ => None,
        }
    }

    pub fn eq(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(BoolObject::new(lhs == rhs).into()),
            (Object::Bool(lhs), Object::Bool(rhs)) => Some(BoolObject::new(lhs == rhs).into()),
            (Object::String(lhs), Object::String(rhs)) => Some(BoolObject::new(lhs == rhs).into()),
            _ => None,
        }
    }

    pub fn not_eq(&self, rhs: &Self) -> Option<Object> {
        self.eq(rhs).map(|b| {
            if let Object::Bool(b) = b {
                b.bang().into()
            } else {
                panic!("equal should return bool object");
            }
        })
    }

    pub fn lt(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs < rhs))),
            _ => None,
        }
    }

    pub fn lt_eq(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs <= rhs))),
            _ => None,
        }
    }

    pub fn gt(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs > rhs))),
            _ => None,
        }
    }

    pub fn gt_eq(&self, rhs: &Self) -> Option<Object> {
        match (self, rhs) {
            (Object::Int(lhs), Object::Int(rhs)) => Some(Object::Bool(BoolObject::new(lhs >= rhs))),
            _ => None,
        }
    }

    pub fn negate(&self) -> Option<Object> {
        match self {
            Object::Int(int_obj) => Some(Object::Int(int_obj.negate())),
            _ => None,
        }
    }

    pub fn bang(&self) -> Option<Object> {
        match self {
            Object::Bool(bool_obj) => Some(Object::Bool(bool_obj.bang())),
            _ => None,
        }
    }

    pub fn into_u16(self) -> Option<u16> {
        match self {
            Object::Int(int_obj) => Some(int_obj.val as u16),
            _ => None,
        }
    }
}

impl Default for Object {
    fn default() -> Self {
        Object::Null
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Int(int_object) => int_object.fmt(f),
            Object::Bool(bool_object) => bool_object.fmt(f),
            Object::String(string_object) => string_object.fmt(f),
            Object::Return(return_object) => return_object.fmt(f),
            Object::Null => {
                write!(f, "null")
            }
            Object::Fn(fn_object) => fn_object.fmt(f),
            Object::Array(array_object) => array_object.fmt(f),
            Object::Hash(hash_object) => hash_object.fmt(f),
            Object::CompiledFn(compiled_fn_object) => compiled_fn_object.fmt(f),
        }
    }
}

#[derive(Debug)]
pub struct ObjectConvertError {
    pub original_type: String,
    pub destinatin_type: String,
}

impl TryInto<BoolObject> for Object {
    type Error = ObjectConvertError;

    fn try_into(self) -> Result<BoolObject, Self::Error> {
        match self {
            Object::Null => Ok(BoolObject::new(false)),
            Object::Int(i) => {
                let b = i.val != 0;
                Ok(BoolObject::new(b))
            }
            Object::Bool(b) => Ok(b),
            Object::Return(ret) => {
                let val = ret.val;
                let obj = *val;
                obj.try_into()
            }
            Object::String(string_obj) => Err(ObjectConvertError {
                original_type: format!("\"{}\"", string_obj.val),
                destinatin_type: "BoolObject".to_string(),
            }),
            Object::Fn(_) => Err(ObjectConvertError {
                original_type: "Fn".to_string(),
                destinatin_type: "BoolObject".to_string(),
            }),
            Object::Array(_) => Err(ObjectConvertError {
                original_type: "Array".to_string(),
                destinatin_type: "BoolObject".to_string(),
            }),
            Object::Hash(_) => Err(ObjectConvertError {
                original_type: "Hash".to_string(),
                destinatin_type: "BoolObject".to_string(),
            }),
            Object::CompiledFn(_) => Err(ObjectConvertError {
                original_type: "CompiledFn".to_string(),
                destinatin_type: "BoolObject".to_string(),
            }),
        }
    }
}

impl TryInto<IntObject> for Object {
    type Error = ObjectConvertError;

    fn try_into(self) -> Result<IntObject, Self::Error> {
        match self {
            Object::Int(i) => Ok(i),
            _ => Err(ObjectConvertError {
                original_type: self.to_string(),
                destinatin_type: "IntObject".to_string(),
            }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IntObject {
    pub val: i32,
}

impl IntObject {
    pub fn new(val: i32) -> Self {
        Self { val }
    }

    pub fn negate(&self) -> Self {
        Self { val: -self.val }
    }
}

impl Add for &IntObject {
    type Output = IntObject;

    fn add(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val + rhs.val)
    }
}

impl Sub for &IntObject {
    type Output = IntObject;

    fn sub(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val - rhs.val)
    }
}

impl Mul for &IntObject {
    type Output = IntObject;

    fn mul(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val * rhs.val)
    }
}

impl Div for &IntObject {
    type Output = IntObject;

    fn div(self, rhs: Self) -> Self::Output {
        IntObject::new(self.val / rhs.val)
    }
}

impl Display for IntObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

impl From<i32> for IntObject {
    fn from(val: i32) -> Self {
        IntObject::new(val)
    }
}

impl From<IntObject> for Object {
    fn from(i: IntObject) -> Self {
        Object::Int(i)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BoolObject {
    pub val: bool,
}

impl BoolObject {
    pub fn new(val: bool) -> Self {
        Self { val }
    }

    pub fn bang(&self) -> Self {
        Self { val: !self.val }
    }
}

impl From<BoolObject> for Object {
    fn from(b: BoolObject) -> Self {
        Object::Bool(b)
    }
}

impl Display for BoolObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.val)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringObject {
    pub val: String,
}

impl StringObject {
    pub fn new(val: String) -> Self {
        Self { val }
    }
}

impl From<StringObject> for Object {
    fn from(b: StringObject) -> Self {
        Object::String(b)
    }
}

impl Display for StringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.val)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayObject {
    pub array: Vec<Object>,
}

impl ArrayObject {
    pub fn new() -> Self {
        Self { array: Vec::new() }
    }
}

impl<It> From<It> for ArrayObject
where
    It: IntoIterator<Item = Object>,
{
    fn from(it: It) -> Self {
        Self {
            array: it.into_iter().collect(),
        }
    }
}

impl Display for ArrayObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (idx, item) in self.array.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            item.fmt(f)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashObject {
    pub hash: HashMap<Object, Object>,
}

impl HashObject {
    pub fn new() -> Self {
        Self {
            hash: HashMap::new(),
        }
    }
}

impl Hash for HashObject {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("hash object can not be hash key");
    }
}

impl<It> From<It> for HashObject
where
    It: IntoIterator<Item = (Object, Object)>,
{
    fn from(it: It) -> Self {
        Self {
            hash: it.into_iter().collect(),
        }
    }
}

impl Display for HashObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (idx, (key, value)) in self.hash.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            key.fmt(f)?;
            write!(f, ":")?;
            value.fmt(f)?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct CompiledFnObject {
    pub instructions: code::Instructions,
    pub symbol_count: u16,
}

impl CompiledFnObject {
    pub fn new(instructions: code::Instructions, symbol_count: u16) -> Self {
        Self {
            instructions,
            symbol_count,
        }
    }
}

impl Display for CompiledFnObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let disassembled = compiler::disassemble(&self.instructions);
        if let Ok(disassembled) = disassembled {
            write!(f, "{}", disassembled)
        } else {
            Err(std::fmt::Error)
        }
    }
}

impl std::fmt::Debug for CompiledFnObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let disassembled =
            compiler::disassemble(&self.instructions).unwrap_or(format!("{:?}", self.instructions));
        f.debug_struct("CompiledFnObject")
            .field("instructions", &disassembled)
            .finish()
    }
}

impl From<(Vec<code::Instructions>, u16)> for CompiledFnObject {
    fn from((codes, symbol_count): (Vec<code::Instructions>, u16)) -> Self {
        let instructions = codes.into_iter().flatten().collect();
        Self {
            instructions,
            symbol_count,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnObject {
    pub val: Box<Object>,
}

impl ReturnObject {
    pub fn new(val: Object) -> Self {
        Self { val: Box::new(val) }
    }
}

impl Display for ReturnObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &*self.val)
    }
}

impl From<ReturnObject> for Object {
    fn from(ret: ReturnObject) -> Self {
        Object::Return(ret)
    }
}

#[derive(Debug, Clone)]
pub struct FnObject {
    pub args: Vec<IdentToken>,
    pub body: Statement, // should be BlockStatement,
}

impl Hash for FnObject {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("FnObject can not be Hashed");
    }
}

impl std::cmp::PartialEq for FnObject {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl std::cmp::Eq for FnObject {
    fn assert_receiver_is_total_eq(&self) {}
}

impl Display for FnObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn (")?;
        for (idx, param) in self.args.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param.to_string())?;
        }

        write!(f, ") {{")?;

        self.body.fmt(f)
    }
}

impl From<FnObject> for Object {
    fn from(fn_obj: FnObject) -> Self {
        Object::Fn(fn_obj)
    }
}
