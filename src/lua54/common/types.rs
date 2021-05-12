use crate::{
	common::types::{Instruction, Integer, Local, Number, Upvalue, Value},
	ext_operand, ext_s_operand,
};
use bit_field::BitField;
use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};
use std::convert::TryFrom;

pub const LUA_MAGIC: &[u8] = b"\x1BLua\x54\x00";
pub const LUA_DATA: &[u8] = b"\x19\x93\r\n\x1a\n";
pub const LUA_INT: Integer = 0x5678;
pub const LUA_NUM: Number = 370.5;

#[derive(Clone, Copy)]
pub struct Inst {
	pub inner: Instruction,
}

impl Inst {
	pub fn iax(op: Opcode, ax: u32) -> Self {
		Self::from(op).set_ax(ax)
	}

	pub fn iabc<A: Into<u8>, B: Into<u8>, C: Into<u8>>(op: Opcode, a: A, b: B, c: C) -> Self {
		Self::from(op)
			.set_a(a.into())
			.set_b(b.into())
			.set_c(c.into())
	}

	pub fn iabsc<A: Into<u8>, B: Into<u8>>(op: Opcode, a: A, b: B, c: i8) -> Self {
		Self::from(op).set_a(a.into()).set_b(b.into()).set_sc(c)
	}

	pub fn iasbc<A: Into<u8>, C: Into<u8>>(op: Opcode, a: A, b: i8, c: C) -> Self {
		Self::from(op).set_a(a.into()).set_sb(b).set_c(c.into())
	}

	pub fn iabx<A: Into<u8>>(op: Opcode, a: A, bx: u32) -> Self {
		Self::from(op).set_a(a.into()).set_bx(bx)
	}

	pub fn iasbx<A: Into<u8>>(op: Opcode, a: A, sbx: i32) -> Self {
		Self::from(op).set_a(a.into()).set_sbx(sbx)
	}

	pub fn isj(op: Opcode, sj: i32) -> Self {
		Self::from(op).set_sj(sj)
	}

	pub fn opcode(self) -> Opcode {
		Opcode::from(u8::try_from(self.inner.get_bits(0..7)).unwrap())
	}

	pub fn k(self) -> bool {
		self.inner.get_bit(15)
	}

	pub fn set_k(mut self, value: bool) -> Self {
		self.inner.set_bit(15, value);
		self
	}

	ext_operand!(a, set_a, u8, 7..15);
	ext_operand!(ax, set_ax, u32, 7..32);
	ext_operand!(b, set_b, u8, 16..24);
	ext_operand!(bx, set_bx, u32, 15..32);
	ext_operand!(c, set_c, u8, 24..32);
	ext_s_operand!(sb, set_sb, i8, 16..24);
	ext_s_operand!(sbx, set_sbx, i32, 15..32);
	ext_s_operand!(sc, set_sc, i8, 24..32);
	ext_s_operand!(sj, set_sj, i32, 7..32);
}

impl From<Opcode> for Inst {
	fn from(op: Opcode) -> Self {
		let inner = u8::from(op).into();

		Self { inner }
	}
}

impl Default for Inst {
	fn default() -> Self {
		Self::from(Opcode::Invalid)
	}
}

#[derive(TryFromPrimitive, IntoPrimitive, PartialEq, Eq)]
#[repr(u8)]
pub enum Constant {
	Nil = 0b00000,
	False = 0b00001,
	True = 0b10001,
	Integer = 0b00011,
	Number = 0b10011,
	ShortString = 0b00100,
	LongString = 0b10100,
}

#[derive(FromPrimitive, IntoPrimitive, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
	Move = 0,
	LoadI,
	LoadF,
	LoadK,
	LoadKX,
	LoadFalse,
	LFalseSkip,
	LoadTrue,
	LoadNil,
	GetUpval,
	SetUpval,

	GetTabUp,
	GetTable,
	GetI,
	GetField,

	SetTabUp,
	SetTable,
	SetI,
	SetField,

	NewTable,

	Method,

	AddI,

	AddK,
	SubK,
	MulK,
	ModK,
	PowK,
	DivK,
	IDivK,

	BandK,
	BorK,
	BxorK,

	ShrI,
	ShlI,

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

	MmBin,
	MmBinI,
	MmBinK,

	Unm,
	Bnot,
	Not,
	Len,

	Concat,

	Close,
	Tbc,
	Jmp,
	Eq,
	Lt,
	Le,

	EqK,
	EqI,
	LtI,
	LeI,
	GtI,
	GeI,

	Test,
	TestSet,

	Call,
	TailCall,

	Return,
	Return0,
	Return1,

	ForLoop,
	ForPrep,

	TForPrep,
	TForCall,
	TForLoop,

	SetList,

	Closure,

	Vararg,
	VarargPrep,

	ExtraArg,

	#[num_enum(default)]
	Invalid,
}

pub struct AbsLine {
	pub pc: u32,
	pub line: u32,
}

impl From<(u32, u32)> for AbsLine {
	fn from(pair: (u32, u32)) -> Self {
		AbsLine {
			pc: pair.0,
			line: pair.1,
		}
	}
}

pub struct Proto {
	pub source: Option<String>,
	pub is_vararg: u8,
	pub num_stack: u8,
	pub num_param: u8,
	pub line_defined: u32,
	pub last_line_defined: u32,
	pub value_list: Vec<Value>,
	pub inst_list: Vec<Inst>,
	pub child_list: Vec<Proto>,
	pub upval_list: Vec<Upvalue>,
	pub rel_line_list: Vec<i8>,
	pub abs_line_list: Vec<AbsLine>,
	pub local_list: Vec<Local>,
}
