use nom::{error::VerboseError, number::Endianness, IResult};
use serde::{Deserialize, Serialize};
use std::rc::Rc;

pub const ENDIANNESS: Endianness = Endianness::Little;

pub type Named<T> = Vec<(Rc<str>, T)>;
pub type Res<'a, T> = IResult<&'a [u8], T, VerboseError<&'a [u8]>>;

pub type Instruction = u32;
pub type Integer = i64;
pub type Number = f64;

#[derive(Deserialize, Serialize)]
pub enum Value {
	Nil,
	False,
	True,
	Integer(Integer),
	Number(Number),
	String(String),
}

impl Value {
	pub fn as_str(&self) -> &str {
		match self {
			Value::Nil => "nil",
			Value::False => "false",
			Value::True => "true",
			Value::Integer(_) => "integer",
			Value::Number(_) => "number",
			Value::String(v) => v.as_ref(),
		}
	}
}

#[derive(Deserialize, Serialize)]
pub struct Local {
	pub name: Option<String>,
	pub start_pc: u32,
	pub end_pc: u32,
}

#[derive(Deserialize, Serialize)]
pub struct Upvalue {
	pub name: Option<String>,
	pub in_stack: bool,
	pub index: u8,
}

#[derive(Deserialize, Serialize)]
pub struct LineInfo {
	pub line_defined: (u32, u32),
	pub line_offset: Vec<i8>,
	pub line_data: Vec<(u32, u32)>,
}

#[derive(Deserialize, Serialize)]
pub struct StackInfo {
	pub is_vararg: u8,
	pub num_stack: u8,
	pub num_param: u8,
}

impl StackInfo {
	pub fn unpack(self) -> (u8, u8, u8) {
		(self.is_vararg, self.num_stack, self.num_param)
	}
}

#[derive(Deserialize, Serialize)]
pub struct Function<B> {
	pub source: Option<String>,
	pub stack_info: StackInfo,
	pub line_info: LineInfo,
	pub value_list: Named<Value>,
	pub local_list: Vec<Local>,
	pub upval_list: Named<Upvalue>,
	pub block_list: Vec<B>,
	pub child_list: Named<Function<B>>,
}
