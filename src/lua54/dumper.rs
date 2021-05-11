use super::common::{
	types::{AbsLine, Constant, Proto},
	varint::dump_unsigned,
};
use crate::{
	common::{
		loader::Serde,
		types::{Instruction, Integer, Local, Number, Upvalue, Value},
	},
	lua54::common::types::{LUA_DATA, LUA_INT, LUA_MAGIC, LUA_NUM},
};
use std::{
	convert::TryFrom,
	io::{Result, Write},
	mem::size_of,
};

fn dump_size_of<T>(w: &mut dyn Write) -> Result<()>
where
	T: Sized,
{
	u8::try_from(size_of::<T>())
		.expect("size of type too large")
		.ser(w)
}

fn dump_lua_header(w: &mut dyn Write) -> Result<()> {
	w.write_all(LUA_MAGIC)?;
	w.write_all(LUA_DATA)?;
	dump_size_of::<Instruction>(w)?;
	dump_size_of::<Integer>(w)?;
	dump_size_of::<Number>(w)?;
	LUA_INT.ser(w)?;
	LUA_NUM.ser(w)?;

	Ok(())
}

fn dump_integer<T>(val: T, w: &mut dyn Write) -> Result<()>
where
	T: Into<u64>,
{
	dump_unsigned(val.into(), w)
}

fn dump_string(val: &str, w: &mut dyn Write) -> Result<()> {
	dump_integer(val.len() as u64 + 1, w)?;
	w.write_all(val.as_bytes())
}

fn dump_opt_string(opt: Option<&str>, w: &mut dyn Write) -> Result<()> {
	match opt {
		Some(s) => dump_string(s, w),
		None => dump_integer(0_u64, w),
	}
}

fn dump_list<T, M>(list: &[T], dump: M, w: &mut dyn Write) -> Result<()>
where
	M: Fn(&T, &mut dyn Write) -> Result<()>,
{
	dump_integer(list.len() as u64, w)?;
	list.iter().try_for_each(|v| dump(v, w))
}

fn dump_constant(value: &Value, w: &mut dyn Write) -> Result<()> {
	match value {
		Value::Nil => u8::from(Constant::Nil).ser(w),
		Value::False => u8::from(Constant::False).ser(w),
		Value::True => u8::from(Constant::True).ser(w),
		Value::Integer(i) => {
			u8::from(Constant::Integer).ser(w)?;
			i.ser(w)
		}
		Value::Number(n) => {
			u8::from(Constant::Number).ser(w)?;
			n.ser(w)
		}
		Value::String(s) => {
			if s.len() < 40 {
				u8::from(Constant::ShortString).ser(w)?;
			} else {
				u8::from(Constant::LongString).ser(w)?;
			}

			dump_string(s, w)
		}
	}
}

fn dump_upval(value: &Upvalue, w: &mut dyn Write) -> Result<()> {
	let in_stack = u8::from(value.in_stack);
	let index = value.index;

	in_stack.ser(w)?;
	index.ser(w)?;
	0_u8.ser(w)?;

	Ok(())
}

fn dump_abs_line(abs: &AbsLine, w: &mut dyn Write) -> Result<()> {
	dump_integer(abs.pc, w)?;
	dump_integer(abs.line, w)?;

	Ok(())
}

fn dump_local(local: &Local, w: &mut dyn Write) -> Result<()> {
	dump_opt_string(local.name.as_deref(), w)?;
	dump_integer(local.start_pc, w)?;
	dump_integer(local.end_pc, w)?;

	Ok(())
}

fn dump_function(proto: &Proto, w: &mut dyn Write) -> Result<()> {
	let upval_name_list: Vec<_> = proto.upval_list.iter().map(|v| v.name.as_deref()).collect();

	dump_opt_string(proto.source.as_deref(), w)?;
	dump_integer(proto.line_defined, w)?;
	dump_integer(proto.last_line_defined, w)?;

	proto.num_param.ser(w)?;
	proto.is_vararg.ser(w)?;
	proto.num_stack.ser(w)?;

	dump_list(&proto.inst_list, |v, w| v.inner.ser(w), w)?;
	dump_list(&proto.value_list, dump_constant, w)?;
	dump_list(&proto.upval_list, dump_upval, w)?;
	dump_list(&proto.child_list, dump_function, w)?;

	dump_list(&proto.rel_line_list, |v, w| v.ser(w), w)?;
	dump_list(&proto.abs_line_list, dump_abs_line, w)?;
	dump_list(&proto.local_list, dump_local, w)?;
	dump_list(&upval_name_list, |v, w| dump_opt_string(*v, w), w)?;

	Ok(())
}

pub fn dump_lua_module(proto: &Proto) -> Result<Vec<u8>> {
	let mut vec = Vec::new();
	let len = proto.upval_list.len();
	let nup = u8::try_from(len).expect("main function too many upvalues (> 255)");

	dump_lua_header(&mut vec)?;
	nup.ser(&mut vec)?;
	dump_function(proto, &mut vec)?;

	Ok(vec)
}
