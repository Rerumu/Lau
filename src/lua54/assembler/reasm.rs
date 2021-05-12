use crate::{
	common::types::Function,
	lua54::common::{
		inst::{Block, Condition, Control, Loop, RegOrK, Target, IR},
		types::{Inst, Opcode, Proto},
	},
};
use std::{cmp::Ordering, collections::HashMap, convert::TryInto, rc::Rc};

struct Translator {
	func_map: HashMap<Rc<str>, u32>,
	upvalue_map: HashMap<Rc<str>, u32>,
	value_map: HashMap<Rc<str>, u32>,
}

enum Remap {
	Fallthrough,
	LFalseSkip {
		reg: u8,
		jump: Option<Target>,
	},
	Loop {
		inst: Inst,
		jump: Target,
		fall: Option<Target>,
	},
	Condition {
		cmp: Inst,
		jump: Option<Target>,
		fall: Option<Target>,
	},
	Unconditional {
		target: Target,
	},
	Return {
		inst: Inst,
	},
}

struct Pending {
	position: usize,
	target: Target,
}

struct Controller {
	inst_list: Vec<Inst>,
	label_map: HashMap<u32, i32>,
	post_list: Vec<Pending>,
}

impl Controller {
	fn new() -> Self {
		Self {
			inst_list: Vec::new(),
			label_map: HashMap::new(),
			post_list: Vec::new(),
		}
	}

	fn add_jump(&mut self, target: Target) {
		let pend = Pending {
			position: self.inst_list.len(),
			target,
		};

		self.add(Opcode::Jmp.into());
		self.post_list.push(pend);
	}

	fn add(&mut self, inst: Inst) {
		self.inst_list.push(inst);
	}

	fn add_targeted(&mut self, inst: Inst, target: Target) {
		self.add_jump(target);
		*self.inst_list.last_mut().unwrap() = inst;
	}

	fn add_odd_loop(&mut self, inst: Inst, jump: Target, id: u32) {
		let has_id = self.label_map.contains_key(&id);

		match inst.opcode() {
			Opcode::ForLoop | Opcode::TForLoop if !has_id => {
				self.add_jump(Target::Undefined(1));
				self.add_jump(jump);
				self.add_targeted(inst, Target::Undefined(-2));
			}
			Opcode::ForPrep if has_id => {
				self.add_targeted(inst, Target::Undefined(0));
				self.add_jump(jump);
			}
			Opcode::TForPrep if has_id => {
				self.add_targeted(inst, Target::Undefined(1));
				self.add_jump(jump);
			}
			_ => {
				self.add_targeted(inst, jump);
			}
		}
	}

	fn redirect_jump_list(&mut self) {
		for pend in &self.post_list {
			let offset = match pend.target {
				Target::Label(id) => self.label_map[&id] - pend.position as i32 - 1,
				Target::Undefined(offset) => offset,
			};

			let repl = match self.inst_list[pend.position].opcode() {
				Opcode::Jmp => Inst::isj(Opcode::Jmp, offset),
				Opcode::ForLoop | Opcode::TForLoop => {
					let inv = (-offset)
						.try_into()
						.expect("ForLoop and TForLoop must jump backward");

					Inst::iabx(0.into(), 0, inv)
				}
				Opcode::ForPrep => {
					let inv: u32 = offset.try_into().expect("ForPrep must jump forward");

					Inst::iabx(0.into(), 0, inv - 1)
				}
				Opcode::TForPrep => {
					let inv = offset.try_into().expect("TForPrep must jump forward");

					Inst::iabx(0.into(), 0, inv)
				}
				_ => unreachable!(),
			};

			self.inst_list[pend.position].inner |= repl.inner;
		}
	}
}

fn wrap_vec(vec: Vec<Rc<str>>) -> HashMap<Rc<str>, u32> {
	vec.into_iter()
		.enumerate()
		.map(|(i, v)| (v, i as u32))
		.collect()
}

// if we have a trailing block and our jump leads to it it's a fallthrough
fn has_fallthrough(target: &Target, next: Option<u32>) -> bool {
	match (target, next) {
		(Target::Label(id), Some(next)) => *id == next,
		(Target::Undefined(offset), _) => *offset == 0,
		_ => false,
	}
}

// if we have an empty block ending in an unconditional it's a jump
fn has_jump(trail: Option<&Block>) -> bool {
	trail
		.map(|v| v.body.is_empty() && matches!(v.edge, Control::Unconditional(_)))
		.unwrap_or_default()
}

// if our blocks point to the same output and the next block only has 1 instruction
// we can reasonably emit a LFALSESKIP
fn has_skip_target(target: &Target, trail: Option<&Block>) -> bool {
	if let (&Target::Label(id), Some(trail)) = (target, trail) {
		if let Control::Unconditional(Target::Label(label)) = trail.edge {
			return trail.body.len() == 1 && id == label;
		}
	}

	false
}

impl Translator {
	fn new(child_name: Vec<Rc<str>>, upval_name: Vec<Rc<str>>, value_name: Vec<Rc<str>>) -> Self {
		let func_map = wrap_vec(child_name);
		let upvalue_map = wrap_vec(upval_name);
		let value_map = wrap_vec(value_name);

		Self {
			func_map,
			upvalue_map,
			value_map,
		}
	}

	fn get_func_index(&self, name: &str) -> u32 {
		self.func_map.get(name).copied().unwrap()
	}

	fn get_upval_index(&self, name: &str) -> u8 {
		self.upvalue_map
			.get(name)
			.copied()
			.unwrap_or_else(|| panic!("`{}` is not a valid upvalue", name))
			.try_into()
			.unwrap_or_else(|_| panic!("`{}` has too large an index (> 255)", name))
	}

	fn get_val_index(&self, name: &str) -> u32 {
		self.value_map
			.get(name)
			.copied()
			.unwrap_or_else(|| panic!("`{}` is not a valid value", name))
	}

	fn get_reg_val_index(&self, name: &str) -> u8 {
		self.get_val_index(name)
			.try_into()
			.unwrap_or_else(|_| panic!("`{}` has too large an index (> 255)", name))
	}

	fn get_reg_k_index(&self, rk: RegOrK) -> (u8, bool) {
		match rk {
			RegOrK::R(reg) => (reg, false),
			RegOrK::K(k) => (self.get_reg_val_index(&k), true),
		}
	}

	fn translate_ir(&self, ir: IR) -> Inst {
		match ir {
			IR::Move(a, b) => Inst::iabc(Opcode::Move, a, b, 0),
			IR::LoadI(a, b) => Inst::iasbx(Opcode::LoadI, a, b),
			IR::LoadF(a, b) => Inst::iasbx(Opcode::LoadF, a, b),
			IR::LoadK(a, b) => Inst::iabx(Opcode::LoadK, a, self.get_val_index(&b)),
			IR::LoadKX(a) => Inst::iabc(Opcode::LoadKX, a, 0, 0),
			IR::LoadFalse(a) => Inst::iabc(Opcode::LoadFalse, a, 0, 0),
			IR::LoadTrue(a) => Inst::iabc(Opcode::LoadTrue, a, 0, 0),
			IR::LoadNil(a, b) => Inst::iabc(Opcode::LoadNil, a, b, 0),
			IR::GetUpval(a, b) => Inst::iabc(Opcode::GetUpval, a, self.get_upval_index(&b), 0),
			IR::SetUpval(a, b) => Inst::iabc(Opcode::SetUpval, a, self.get_upval_index(&b), 0),
			IR::GetTabUp(a, b, c) => Inst::iabc(
				Opcode::GetTabUp,
				a,
				self.get_upval_index(&b),
				self.get_reg_val_index(&c),
			),
			IR::GetTable(a, b, c) => Inst::iabc(Opcode::GetTable, a, b, c),
			IR::GetI(a, b, c) => Inst::iabc(Opcode::GetI, a, b, c),
			IR::GetField(a, b, c) => Inst::iabc(Opcode::GetField, a, b, self.get_reg_val_index(&c)),
			IR::SetTabUp(a, b, c) => {
				let (c, k) = self.get_reg_k_index(c);

				Inst::iabc(
					Opcode::SetTabUp,
					self.get_upval_index(&a),
					self.get_reg_val_index(&b),
					c,
				)
				.set_k(k)
			}
			IR::SetTable(a, b, c) => {
				let (c, k) = self.get_reg_k_index(c);

				Inst::iabc(Opcode::SetTable, a, b, c).set_k(k)
			}
			IR::SetI(a, b, c) => {
				let (c, k) = self.get_reg_k_index(c);

				Inst::iabc(Opcode::SetI, a, b, c).set_k(k)
			}
			IR::SetField(a, b, c) => {
				let (c, k) = self.get_reg_k_index(c);

				Inst::iabc(Opcode::SetField, a, self.get_reg_val_index(&b), c).set_k(k)
			}
			IR::NewTable(a, b, c, k) => Inst::iabc(Opcode::NewTable, a, b, c).set_k(k),
			IR::Method(a, b, c) => {
				let (c, k) = self.get_reg_k_index(c);

				Inst::iabc(Opcode::Method, a, b, c).set_k(k)
			}
			IR::AddI(a, b, c) => Inst::iabsc(Opcode::AddI, a, b, c),
			IR::AddK(a, b, c) => Inst::iabc(Opcode::AddK, a, b, self.get_reg_val_index(&c)),
			IR::SubK(a, b, c) => Inst::iabc(Opcode::SubK, a, b, self.get_reg_val_index(&c)),
			IR::MulK(a, b, c) => Inst::iabc(Opcode::MulK, a, b, self.get_reg_val_index(&c)),
			IR::ModK(a, b, c) => Inst::iabc(Opcode::ModK, a, b, self.get_reg_val_index(&c)),
			IR::PowK(a, b, c) => Inst::iabc(Opcode::PowK, a, b, self.get_reg_val_index(&c)),
			IR::DivK(a, b, c) => Inst::iabc(Opcode::DivK, a, b, self.get_reg_val_index(&c)),
			IR::IDivK(a, b, c) => Inst::iabc(Opcode::IDivK, a, b, self.get_reg_val_index(&c)),
			IR::BandK(a, b, c) => Inst::iabc(Opcode::BandK, a, b, self.get_reg_val_index(&c)),
			IR::BorK(a, b, c) => Inst::iabc(Opcode::BorK, a, b, self.get_reg_val_index(&c)),
			IR::BxorK(a, b, c) => Inst::iabc(Opcode::BxorK, a, b, self.get_reg_val_index(&c)),
			IR::ShrI(a, b, c) => Inst::iabsc(Opcode::ShrI, a, b, c),
			IR::ShlI(a, b, c) => Inst::iabsc(Opcode::ShlI, a, b, c),
			IR::Add(a, b, c) => Inst::iabc(Opcode::Add, a, b, c),
			IR::Sub(a, b, c) => Inst::iabc(Opcode::Sub, a, b, c),
			IR::Mul(a, b, c) => Inst::iabc(Opcode::Mul, a, b, c),
			IR::Mod(a, b, c) => Inst::iabc(Opcode::Mod, a, b, c),
			IR::Pow(a, b, c) => Inst::iabc(Opcode::Pow, a, b, c),
			IR::Div(a, b, c) => Inst::iabc(Opcode::Div, a, b, c),
			IR::IDiv(a, b, c) => Inst::iabc(Opcode::IDiv, a, b, c),
			IR::Band(a, b, c) => Inst::iabc(Opcode::Band, a, b, c),
			IR::Bor(a, b, c) => Inst::iabc(Opcode::Bor, a, b, c),
			IR::Bxor(a, b, c) => Inst::iabc(Opcode::Bxor, a, b, c),
			IR::Shl(a, b, c) => Inst::iabc(Opcode::Shl, a, b, c),
			IR::Shr(a, b, c) => Inst::iabc(Opcode::Shr, a, b, c),
			IR::MmBin(a, b, c) => Inst::iabc(Opcode::MmBin, a, b, c),
			IR::MmBinI(a, b, c, k) => Inst::iasbc(Opcode::MmBinI, a, b, c).set_k(k),
			IR::MmBinK(a, b, c, k) => {
				Inst::iabc(Opcode::MmBinK, a, self.get_reg_val_index(&b), c).set_k(k)
			}
			IR::Unm(a, b) => Inst::iabc(Opcode::Unm, a, b, 0),
			IR::Bnot(a, b) => Inst::iabc(Opcode::Bnot, a, b, 0),
			IR::Not(a, b) => Inst::iabc(Opcode::Not, a, b, 0),
			IR::Len(a, b) => Inst::iabc(Opcode::Len, a, b, 0),
			IR::Concat(a, b, c) => Inst::iabc(Opcode::Concat, a, b, c),
			IR::Close(a) => Inst::iabc(Opcode::Close, a, 0, 0),
			IR::Tbc(a) => Inst::iabc(Opcode::Tbc, a, 0, 0),
			IR::Call(a, b, c) => Inst::iabc(Opcode::Call, a, b, c),
			IR::TailCall(a, b, c) => Inst::iabc(Opcode::TailCall, a, b, c),
			IR::TForCall(a, c) => Inst::iabc(Opcode::TForCall, a, 0, c),
			IR::SetList(a, b, c, k) => Inst::iabc(Opcode::SetList, a, b, c).set_k(k),
			IR::Closure(a, b) => Inst::iabx(Opcode::Closure, a, self.get_func_index(&b)),
			IR::Vararg(a, b) => Inst::iabc(Opcode::Vararg, a, b, 0),
			IR::VarargPrep(a) => Inst::iabc(Opcode::VarargPrep, a, 0, 0),
			IR::ExtraInteger(a) => Inst::iax(Opcode::ExtraArg, a),
			IR::ExtraValue(a) => Inst::iax(Opcode::ExtraArg, self.get_val_index(&a)),
			IR::Invalid(inner) => Inst { inner },
		}
	}

	fn translate_condition(&self, cond: Condition) -> Inst {
		match cond {
			Condition::Test(a) => Inst::iabc(Opcode::Test, a, 0, 0),
			Condition::TestSet(a, b) => Inst::iabc(Opcode::TestSet, a, b, 0),
			Condition::Eq(a, b) => Inst::iabc(Opcode::Eq, a, b, 0),
			Condition::EqI(a, b) => Inst::iasbc(Opcode::EqI, a, b, 0),
			Condition::EqK(a, b) => Inst::iabc(Opcode::EqK, a, self.get_reg_val_index(&b), 0),
			Condition::GeI(a, b) => Inst::iasbc(Opcode::GeI, a, b, 0),
			Condition::GtI(a, b) => Inst::iasbc(Opcode::GtI, a, b, 0),
			Condition::Le(a, b) => Inst::iabc(Opcode::Le, a, b, 0),
			Condition::LeI(a, b) => Inst::iasbc(Opcode::LeI, a, b, 0),
			Condition::Lt(a, b) => Inst::iabc(Opcode::Lt, a, b, 0),
			Condition::LtI(a, b) => Inst::iasbc(Opcode::LtI, a, b, 0),
		}
	}

	fn translate_loop(cond: Loop) -> Inst {
		match cond {
			Loop::Iterator(a) => Inst::iabc(Opcode::TForLoop, a, 0, 0),
			Loop::IteratorPrep(a) => Inst::iabc(Opcode::TForPrep, a, 0, 0),
			Loop::Numeric(a) => Inst::iabc(Opcode::ForLoop, a, 0, 0),
			Loop::NumericPrep(a) => Inst::iabc(Opcode::ForPrep, a, 0, 0),
		}
	}

	fn translate_control(&self, ctrl: Control, trail: Option<&Block>) -> Remap {
		let next = trail.map(|v| v.label);

		match ctrl {
			Control::LFalseSkip(a, jump) => {
				let jump = if has_skip_target(&jump, trail) {
					None
				} else {
					Some(jump)
				};

				Remap::LFalseSkip {
					reg: a.into(),
					jump,
				}
			}
			Control::Condition(cond, a, b) => {
				let mut cmp = self.translate_condition(cond);
				let (jump, fall) = match (has_fallthrough(&a, next), has_fallthrough(&b, next)) {
					(true, false) => {
						cmp = cmp.set_k(true);

						// if the trailing block is a single jump then adopt it
						if has_jump(trail) {
							(None, None)
						} else {
							(Some(a), None)
						}
					}
					(false, true) => {
						if has_jump(trail) {
							(None, None)
						} else {
							(Some(b), None)
						}
					}
					(true, true) => (Some(b), None),
					(false, false) => (Some(b), Some(a)),
				};

				Remap::Condition { cmp, jump, fall }
			}
			Control::Loop(cond, a, jump) => {
				let inst = Self::translate_loop(cond);
				let fall = if has_fallthrough(&a, next) {
					None
				} else {
					Some(a)
				};

				Remap::Loop { inst, jump, fall }
			}
			Control::Return0 => Remap::Return {
				inst: Opcode::Return0.into(),
			},
			Control::Return1(a) => Remap::Return {
				inst: Inst::iabc(Opcode::Return1, a, 0, 0),
			},
			Control::Return(a, b, c, k) => Remap::Return {
				inst: Inst::iabc(Opcode::Return, a, b, c).set_k(k),
			},
			Control::Unconditional(target) => {
				if has_fallthrough(&target, next) {
					Remap::Fallthrough
				} else {
					Remap::Unconditional { target }
				}
			}
		}
	}

	fn translate(self, block_list: Vec<Block>) -> Vec<Inst> {
		let mut control = Controller::new();
		let mut iter = block_list.into_iter().peekable();

		// `0` entry point must be present
		match iter.peek().map(|v| v.label) {
			Some(0) | None => {}
			Some(_) => {
				// jump to entry
				control.add_jump(Target::Label(0));
			}
		}

		while let Some(blk) = iter.next() {
			control
				.label_map
				.insert(blk.label, control.inst_list.len() as i32);
			control
				.inst_list
				.extend(blk.body.into_iter().map(|v| self.translate_ir(v)));

			match self.translate_control(blk.edge, iter.peek()) {
				Remap::Fallthrough => {}
				Remap::LFalseSkip { reg, jump } => match jump {
					Some(jump) => {
						control.add(Inst::iabc(Opcode::LoadFalse, reg, 0, 0));
						control.add_jump(jump);
					}
					None => {
						control.add(Inst::iabc(Opcode::LFalseSkip, reg, 0, 0));
					}
				},
				Remap::Loop { inst, jump, fall } => {
					if let Target::Label(id) = jump {
						control.add_odd_loop(inst, jump, id);
					} else {
						control.add_targeted(inst, jump);
					}

					if let Some(fall) = fall {
						control.add_jump(fall);
					}
				}
				Remap::Condition { cmp, jump, fall } => {
					control.add(cmp);

					if let Some(jump) = jump {
						control.add_jump(jump);
					}

					if let Some(fall) = fall {
						control.add_jump(fall);
					}
				}
				Remap::Unconditional { target } => {
					control.add_jump(target);
				}
				Remap::Return { inst } => {
					control.add(inst);
				}
			}
		}

		control.redirect_jump_list();
		control.inst_list.shrink_to_fit();
		control.inst_list
	}
}

fn align_rel_line_list(line_list: &mut Vec<i8>, len: usize) {
	match line_list.len().cmp(&len) {
		Ordering::Equal => {}
		Ordering::Less => {
			let iter = std::iter::repeat(0).take(len - line_list.len());

			line_list.extend(iter);
		}
		Ordering::Greater => {
			line_list.truncate(len);
		}
	}
}

impl From<Function<Block>> for Proto {
	fn from(func: Function<Block>) -> Self {
		let (child_name, child_list): (_, Vec<_>) = func.child_list.into_iter().unzip();
		let (upval_name, upval_list) = func.upval_list.into_iter().unzip();
		let (value_name, value_list) = func.value_list.into_iter().unzip();

		let inst_list =
			Translator::new(child_name, upval_name, value_name).translate(func.block_list);

		let source = func.source;
		let local_list = func.local_list;
		let child_list = child_list.into_iter().map(Self::from).collect();

		let (is_vararg, num_stack, num_param) = func.stack_info.unpack();
		let (line_defined, last_line_defined) = func.line_info.line_defined;

		let mut rel_line_list = func.line_info.line_offset;
		let abs_line_list = func
			.line_info
			.line_data
			.into_iter()
			.map(From::from)
			.collect();

		align_rel_line_list(&mut rel_line_list, inst_list.len());

		Proto {
			source,
			is_vararg,
			num_stack,
			num_param,
			line_defined,
			last_line_defined,
			value_list,
			inst_list,
			child_list,
			upval_list,
			rel_line_list,
			abs_line_list,
			local_list,
		}
	}
}
