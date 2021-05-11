use crate::common::types::Instruction;
use num_enum::{FromPrimitive, IntoPrimitive};
use serde::{Deserialize, Serialize};
use std::rc::Rc;

#[derive(Deserialize, Serialize)]
pub enum Reg {
	R(u8),
}

impl From<u8> for Reg {
	fn from(reg: u8) -> Self {
		Self::R(reg)
	}
}

impl From<Reg> for u8 {
	fn from(reg: Reg) -> Self {
		let Reg::R(x) = reg;

		x
	}
}

#[derive(Deserialize, Serialize)]
pub enum RegOrK {
	R(u8),
	K(Rc<str>),
}

impl From<u8> for RegOrK {
	fn from(reg: u8) -> Self {
		Self::R(reg)
	}
}

impl From<Rc<str>> for RegOrK {
	fn from(k: Rc<str>) -> Self {
		Self::K(k)
	}
}

#[derive(FromPrimitive, IntoPrimitive, Deserialize, Serialize)]
#[repr(u8)]
pub enum MetaMethod {
	Index = 0,
	NewIndex,
	Gc,
	Mode,
	Len,
	Eq,
	Add,
	Sub,
	Mul,
	Mod,
	Pow,
	Div,
	IDiv,
	Band,
	Bor,
	Bxor,
	Shl,
	Shr,
	Unm,
	Bnot,
	Lt,
	Le,
	Concat,
	Call,
	Close,

	#[num_enum(default)]
	Invalid,
}

#[derive(Deserialize, Serialize)]
pub enum Group {
	Many,
	Exactly(u8),
}

impl From<u8> for Group {
	fn from(value: u8) -> Self {
		match value {
			0 => Group::Many,
			x => Group::Exactly(x - 1),
		}
	}
}

impl From<Group> for u8 {
	fn from(value: Group) -> Self {
		match value {
			Group::Many => 0,
			Group::Exactly(x) => x + 1,
		}
	}
}

#[derive(Deserialize, Serialize)]
pub enum IR {
	Move(Reg, Reg),
	LoadI(Reg, i32),
	LoadF(Reg, i32),
	LoadK(Reg, Rc<str>),
	LoadKX(Reg),
	LoadFalse(Reg),
	LoadTrue(Reg),
	LoadNil(Reg, u8),
	GetUpval(Reg, Rc<str>),
	SetUpval(Reg, Rc<str>),

	GetTabUp(Reg, Rc<str>, Rc<str>),
	GetTable(Reg, Reg, Reg),
	GetI(Reg, Reg, u8),
	GetField(Reg, Reg, Rc<str>),

	SetTabUp(Rc<str>, Rc<str>, RegOrK),
	SetTable(Reg, Reg, RegOrK),
	SetI(Reg, u8, RegOrK),
	SetField(Reg, Rc<str>, RegOrK),

	NewTable(Reg, u8, u8, bool),

	Method(Reg, Reg, RegOrK),

	AddI(Reg, Reg, i8),

	AddK(Reg, Reg, Rc<str>),
	SubK(Reg, Reg, Rc<str>),
	MulK(Reg, Reg, Rc<str>),
	ModK(Reg, Reg, Rc<str>),
	PowK(Reg, Reg, Rc<str>),
	DivK(Reg, Reg, Rc<str>),
	IDivK(Reg, Reg, Rc<str>),

	BandK(Reg, Reg, Rc<str>),
	BorK(Reg, Reg, Rc<str>),
	BxorK(Reg, Reg, Rc<str>),

	ShrI(Reg, Reg, i8),
	ShlI(Reg, Reg, i8),

	Add(Reg, Reg, Reg),
	Sub(Reg, Reg, Reg),
	Mul(Reg, Reg, Reg),
	Mod(Reg, Reg, Reg),
	Pow(Reg, Reg, Reg),
	Div(Reg, Reg, Reg),
	IDiv(Reg, Reg, Reg),

	Band(Reg, Reg, Reg),
	Bor(Reg, Reg, Reg),
	Bxor(Reg, Reg, Reg),
	Shl(Reg, Reg, Reg),
	Shr(Reg, Reg, Reg),

	MmBin(Reg, Reg, MetaMethod),
	MmBinI(Reg, i8, MetaMethod, bool),
	MmBinK(Reg, Rc<str>, MetaMethod, bool),

	Unm(Reg, Reg),
	Bnot(Reg, Reg),
	Not(Reg, Reg),
	Len(Reg, Reg),

	Concat(Reg, Reg, u8),

	Close(Reg),
	Tbc(Reg),

	Call(Reg, Group, Group),
	TailCall(Reg, Group, Group),

	TForCall(Reg, u8),

	SetList(Reg, u8, u8, bool),

	Closure(Reg, Rc<str>),

	Vararg(Reg, Group),
	VarargPrep(Reg),

	ExtraInteger(u32),
	ExtraValue(Rc<str>),

	Invalid(Instruction),
}

#[derive(Deserialize, Serialize)]
pub enum Target {
	Label(u32),
	Undefined(i32),
}

#[derive(Deserialize, Serialize)]
pub enum Loop {
	Iterator(Reg),
	IteratorPrep(Reg),
	Numeric(Reg),
	NumericPrep(Reg),
}

#[derive(Deserialize, Serialize)]
pub enum Condition {
	// unop - comparison
	Test(Reg),
	TestSet(Reg, Reg),

	// binop - comparison
	Eq(Reg, Reg),
	EqI(Reg, i8),
	EqK(Reg, Rc<str>),
	GeI(Reg, i8),
	GtI(Reg, i8),
	Le(Reg, Reg),
	LeI(Reg, i8),
	Lt(Reg, Reg),
	LtI(Reg, i8),
}

#[derive(Deserialize, Serialize)]
pub enum Control {
	LFalseSkip(Reg, Target),
	Condition(Condition, Target, Target),
	Loop(Loop, Target, Target),
	Return(Reg, Group, u8, bool),
	Return0,
	Return1(Reg),
	Unconditional(Target),
}

#[derive(Deserialize, Serialize)]
pub struct Block {
	pub label: u32,
	pub body: Vec<IR>,
	pub edge: Control,
}

impl Block {
	pub fn new(label: u32, body: Vec<IR>, edge: Control) -> Self {
		Self { label, body, edge }
	}
}
