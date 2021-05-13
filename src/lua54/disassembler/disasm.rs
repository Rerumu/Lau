use super::splitter::{Block as PreBlock, Splitter};
use crate::{
	common::{
		types::{Function, LineInfo, StackInfo},
		unique::{name_child_list, name_upvalue_list, name_value_list},
	},
	lua54::common::{
		inst::{Block, Condition, Control, Loop, Reg, RegOrK, Target, IR},
		types::{Inst, Opcode, Proto},
	},
};
use std::rc::Rc;

fn swap_if_k(inst: Inst, opt1: Target, opt2: Target) -> (Target, Target) {
	if inst.k() {
		(opt2, opt1)
	} else {
		(opt1, opt2)
	}
}

struct Translator {
	child_list: Vec<Rc<str>>,
	upval_list: Vec<Rc<str>>,
	value_list: Vec<Rc<str>>,
}

impl Translator {
	fn new(child_list: Vec<Rc<str>>, upval_list: Vec<Rc<str>>, value_list: Vec<Rc<str>>) -> Self {
		Self {
			child_list,
			upval_list,
			value_list,
		}
	}

	fn translate(self, pre_list: Vec<PreBlock>) -> Vec<Block> {
		pre_list
			.into_iter()
			.enumerate()
			.map(|v| self.translate_block(v.0 as u32, v.1))
			.collect()
	}

	fn get_val_name<T>(&self, index: T) -> Rc<str>
	where
		T: Into<u32>,
	{
		Rc::clone(&self.value_list[index.into() as usize])
	}

	fn get_upval_name(&self, index: u8) -> Rc<str> {
		Rc::clone(&self.upval_list[index as usize])
	}

	fn get_child_name(&self, index: u32) -> Rc<str> {
		Rc::clone(&self.child_list[index as usize])
	}

	fn get_rk_value(&self, inst: Inst) -> RegOrK {
		let c = inst.c();

		if inst.k() {
			self.get_val_name(c).into()
		} else {
			c.into()
		}
	}

	fn gen_condition_normal(
		inst: Inst,
		on_true: Target,
		on_false: Target,
		func: fn(Reg, Reg) -> Condition,
	) -> Control {
		let (on_true, on_false) = swap_if_k(inst, on_true, on_false);

		Control::Condition(func(inst.a().into(), inst.b().into()), on_true, on_false)
	}

	fn gen_condition_imm(
		inst: Inst,
		on_true: Target,
		on_false: Target,
		func: fn(Reg, i8) -> Condition,
	) -> Control {
		let (on_true, on_false) = swap_if_k(inst, on_true, on_false);

		Control::Condition(func(inst.a().into(), inst.sb()), on_true, on_false)
	}

	fn gen_condition_const(
		&self,
		inst: Inst,
		on_true: Target,
		on_false: Target,
		func: fn(Reg, Rc<str>) -> Condition,
	) -> Control {
		let (on_true, on_false) = swap_if_k(inst, on_true, on_false);

		Control::Condition(
			func(inst.a().into(), self.get_val_name(inst.b())),
			on_true,
			on_false,
		)
	}

	fn gen_loop(inst: Inst, on_true: Target, on_false: Target, func: fn(Reg) -> Loop) -> Control {
		// loops are swapped for readability
		// "true" = no jump
		// "false" = yes jump
		Control::Loop(func(inst.a().into()), on_false, on_true)
	}

	fn translate_control(&self, last: Inst, on_true: Target, on_false: Target) -> Control {
		// farewell, the void is calling
		match last.opcode() {
			Opcode::TForLoop => Self::gen_loop(last, on_true, on_false, Loop::Iterator),
			Opcode::TForPrep => Self::gen_loop(last, on_true, on_false, Loop::IteratorPrep),
			Opcode::ForLoop => Self::gen_loop(last, on_true, on_false, Loop::Numeric),
			Opcode::ForPrep => Self::gen_loop(last, on_true, on_false, Loop::NumericPrep),
			Opcode::LFalseSkip => Control::LFalseSkip(last.a().into(), on_true),
			Opcode::Test => {
				let (on_true, on_false) = swap_if_k(last, on_true, on_false);

				Control::Condition(Condition::Test(last.a().into()), on_true, on_false)
			}
			Opcode::TestSet => {
				let (on_true, on_false) = swap_if_k(last, on_true, on_false);

				Control::Condition(
					Condition::TestSet(last.a().into(), last.b().into()),
					on_true,
					on_false,
				)
			}
			Opcode::Eq => Self::gen_condition_normal(last, on_true, on_false, Condition::Eq),
			Opcode::EqI => Self::gen_condition_imm(last, on_true, on_false, Condition::EqI),
			Opcode::EqK => self.gen_condition_const(last, on_true, on_false, Condition::EqK),
			Opcode::GeI => Self::gen_condition_imm(last, on_true, on_false, Condition::GeI),
			Opcode::GtI => Self::gen_condition_imm(last, on_true, on_false, Condition::GtI),
			Opcode::Le => Self::gen_condition_normal(last, on_true, on_false, Condition::Le),
			Opcode::LeI => Self::gen_condition_imm(last, on_true, on_false, Condition::LeI),
			Opcode::Lt => Self::gen_condition_normal(last, on_true, on_false, Condition::Lt),
			Opcode::LtI => Self::gen_condition_imm(last, on_true, on_false, Condition::LtI),
			Opcode::Return => Control::Return(last.a().into(), last.b().into(), last.c(), last.k()),
			Opcode::Return0 => Control::Return0,
			Opcode::Return1 => Control::Return1(last.a().into()),
			// Opcode::Jmp included
			_ => Control::Unconditional(on_true),
		}
	}

	fn gen_unop_normal(inst: Inst, func: fn(Reg, Reg) -> IR) -> IR {
		func(inst.a().into(), inst.b().into())
	}

	fn gen_binop_imm(inst: Inst, func: fn(Reg, Reg, i8) -> IR) -> IR {
		func(inst.a().into(), inst.b().into(), inst.sc())
	}

	fn gen_binop_const(&self, inst: Inst, func: fn(Reg, Reg, Rc<str>) -> IR) -> IR {
		func(
			inst.a().into(),
			inst.b().into(),
			self.get_val_name(inst.c()),
		)
	}

	fn gen_binop_normal(inst: Inst, func: fn(Reg, Reg, Reg) -> IR) -> IR {
		func(inst.a().into(), inst.b().into(), inst.c().into())
	}

	fn translate_code(&self, code: &[Inst]) -> Vec<IR> {
		let mut list = Vec::new();
		let mut iter = code.iter().copied().peekable();

		while let Some(inst) = iter.next() {
			let op = match inst.opcode() {
				Opcode::Move => IR::Move(inst.a().into(), inst.b().into()),
				Opcode::LoadI => IR::LoadI(inst.a().into(), inst.sbx()),
				Opcode::LoadF => IR::LoadF(inst.a().into(), inst.sbx()),
				Opcode::LoadK => IR::LoadK(inst.a().into(), self.get_val_name(inst.bx())),
				Opcode::LoadKX => {
					let post = iter.peek().copied().unwrap_or_default();

					if post.opcode() == Opcode::ExtraArg {
						iter.next().unwrap();
					}

					list.push(IR::LoadKX(inst.a().into()));
					IR::ExtraValue(self.get_val_name(post.ax()))
				}
				Opcode::LoadFalse => IR::LoadFalse(inst.a().into()),
				Opcode::LoadTrue => IR::LoadTrue(inst.a().into()),
				Opcode::LoadNil => IR::LoadNil(inst.a().into(), inst.b()),
				Opcode::GetUpval => IR::GetUpval(inst.a().into(), self.get_upval_name(inst.b())),
				Opcode::SetUpval => IR::SetUpval(inst.a().into(), self.get_upval_name(inst.b())),
				Opcode::GetTabUp => IR::GetTabUp(
					inst.a().into(),
					self.get_upval_name(inst.b()),
					self.get_val_name(inst.c()),
				),
				Opcode::GetTable => IR::GetTable(inst.a().into(), inst.b().into(), inst.c().into()),
				Opcode::GetI => IR::GetI(inst.a().into(), inst.b().into(), inst.c()),
				Opcode::GetField => IR::GetField(
					inst.a().into(),
					inst.b().into(),
					self.get_val_name(inst.c()),
				),
				Opcode::SetTabUp => IR::SetTabUp(
					self.get_upval_name(inst.a()),
					self.get_val_name(inst.b()),
					self.get_rk_value(inst),
				),
				Opcode::SetTable => {
					IR::SetTable(inst.a().into(), inst.b().into(), self.get_rk_value(inst))
				}
				Opcode::SetI => IR::SetI(inst.a().into(), inst.b(), self.get_rk_value(inst)),
				Opcode::SetField => IR::SetField(
					inst.a().into(),
					self.get_val_name(inst.b()),
					self.get_rk_value(inst),
				),
				Opcode::NewTable => {
					let post = iter.peek().copied().unwrap_or_default();

					list.push(IR::NewTable(inst.a().into(), inst.b(), inst.c(), inst.k()));

					if post.opcode() == Opcode::ExtraArg {
						continue;
					}

					IR::ExtraInteger(post.ax())
				}
				Opcode::Method => {
					IR::Method(inst.a().into(), inst.b().into(), self.get_rk_value(inst))
				}
				Opcode::AddI => Self::gen_binop_imm(inst, IR::AddI),
				Opcode::AddK => self.gen_binop_const(inst, IR::AddK),
				Opcode::SubK => self.gen_binop_const(inst, IR::SubK),
				Opcode::MulK => self.gen_binop_const(inst, IR::MulK),
				Opcode::ModK => self.gen_binop_const(inst, IR::ModK),
				Opcode::PowK => self.gen_binop_const(inst, IR::PowK),
				Opcode::DivK => self.gen_binop_const(inst, IR::DivK),
				Opcode::IDivK => self.gen_binop_const(inst, IR::IDivK),
				Opcode::BandK => self.gen_binop_const(inst, IR::BandK),
				Opcode::BorK => self.gen_binop_const(inst, IR::BorK),
				Opcode::BxorK => self.gen_binop_const(inst, IR::BxorK),
				Opcode::ShrI => Self::gen_binop_imm(inst, IR::ShrI),
				Opcode::ShlI => Self::gen_binop_imm(inst, IR::ShlI),
				Opcode::Add => Self::gen_binop_normal(inst, IR::Add),
				Opcode::Sub => Self::gen_binop_normal(inst, IR::Sub),
				Opcode::Mul => Self::gen_binop_normal(inst, IR::Mul),
				Opcode::Mod => Self::gen_binop_normal(inst, IR::Mod),
				Opcode::Pow => Self::gen_binop_normal(inst, IR::Pow),
				Opcode::Div => Self::gen_binop_normal(inst, IR::Div),
				Opcode::IDiv => Self::gen_binop_normal(inst, IR::IDiv),
				Opcode::Band => Self::gen_binop_normal(inst, IR::Band),
				Opcode::Bor => Self::gen_binop_normal(inst, IR::Bor),
				Opcode::Bxor => Self::gen_binop_normal(inst, IR::Bxor),
				Opcode::Shl => Self::gen_binop_normal(inst, IR::Shl),
				Opcode::Shr => Self::gen_binop_normal(inst, IR::Shr),
				Opcode::MmBin => IR::MmBin(inst.a().into(), inst.b().into(), inst.c().into()),
				Opcode::MmBinI => IR::MmBinI(inst.a().into(), inst.sb(), inst.c().into(), inst.k()),
				Opcode::MmBinK => IR::MmBinK(
					inst.a().into(),
					self.get_val_name(inst.b()),
					inst.c().into(),
					inst.k(),
				),
				Opcode::Unm => Self::gen_unop_normal(inst, IR::Unm),
				Opcode::Bnot => Self::gen_unop_normal(inst, IR::Bnot),
				Opcode::Not => Self::gen_unop_normal(inst, IR::Not),
				Opcode::Len => Self::gen_unop_normal(inst, IR::Len),
				Opcode::Concat => IR::Concat(inst.a().into(), inst.b().into(), inst.c()),
				Opcode::Close => IR::Close(inst.a().into()),
				Opcode::Tbc => IR::Tbc(inst.a().into()),
				Opcode::Call => IR::Call(inst.a().into(), inst.b().into(), inst.c().into()),
				Opcode::TailCall => IR::TailCall(inst.a().into(), inst.b().into(), inst.c().into()),
				Opcode::TForCall => IR::TForCall(inst.a().into(), inst.c()),
				Opcode::SetList => {
					let post = iter.peek().copied().unwrap_or_default();
					let is_ext = inst.k();

					list.push(IR::SetList(inst.a().into(), inst.b(), inst.c(), is_ext));

					if is_ext && post.opcode() != Opcode::ExtraArg {
						IR::ExtraInteger(post.ax())
					} else {
						continue;
					}
				}
				Opcode::Closure => IR::Closure(inst.a().into(), self.get_child_name(inst.bx())),
				Opcode::Vararg => IR::Vararg(inst.a().into(), inst.b().into()),
				Opcode::VarargPrep => IR::VarargPrep(inst.a().into()),
				Opcode::ExtraArg => IR::ExtraInteger(inst.ax()),
				Opcode::Invalid => IR::Invalid(inst.inner),
				_ => {
					continue;
				}
			};

			list.push(op);
		}

		list
	}

	fn translate_block(&self, label: u32, blk: PreBlock) -> Block {
		let last = blk.code.last().copied().unwrap();
		let code = self.translate_code(&blk.code);
		let control = self.translate_control(last, blk.target, Target::Label(label + 1));

		Block::new(label, code, control)
	}
}

fn copy_stack_info(func: &Proto) -> StackInfo {
	StackInfo {
		is_vararg: func.is_vararg,
		num_stack: func.num_stack,
		num_param: func.num_param,
	}
}

// FIXME: Eventually eliminate cloning
fn copy_line_info(func: &Proto) -> LineInfo {
	let line_defined = (func.line_defined, func.last_line_defined);
	let line_offset = func.rel_line_list.clone();
	let line_data = func.abs_line_list.iter().map(|v| (v.pc, v.line)).collect();

	LineInfo {
		line_defined,
		line_offset,
		line_data,
	}
}

impl From<Proto> for Function<Block> {
	fn from(func: Proto) -> Self {
		let stack_info = copy_stack_info(&func);
		let line_info = copy_line_info(&func);

		let source = func.source;
		let local_list = func.local_list;

		let child_list = func.child_list.into_iter().map(Self::from).collect();

		let child_list = name_child_list(child_list);
		let upval_list = name_upvalue_list(func.upval_list);
		let value_list = name_value_list(func.value_list);

		let child_name = child_list.iter().map(|v| Rc::clone(&v.0)).collect();
		let upval_name = upval_list.iter().map(|v| Rc::clone(&v.0)).collect();
		let value_name = value_list.iter().map(|v| Rc::clone(&v.0)).collect();

		let pre_list = Splitter::new().split(func.inst_list);
		let block_list = Translator::new(child_name, upval_name, value_name).translate(pre_list);

		Function {
			source,
			stack_info,
			line_info,
			value_list,
			local_list,
			upval_list,
			block_list,
			child_list,
		}
	}
}
