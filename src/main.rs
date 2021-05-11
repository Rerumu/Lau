use common::types::Function;
use lua54::{
	common::{inst::Block, types::Proto},
	dumper::dump_lua_module,
	loader::load_lua_module,
};
use rand::seq::SliceRandom;
use ron::{
	de::from_bytes,
	ser::{to_string_pretty, PrettyConfig},
};
use std::{
	io::{Result, Write},
	rc::Rc,
};

mod common;
mod lua54;

enum Mutation {
	Random,
	Sorted,
}

fn try_mutate(func: &mut Function<Block>, opt: &[Mutation]) {
	let mut rng = rand::thread_rng();

	for data in &mut func.child_list {
		try_mutate(&mut data.1, opt);
	}

	for step in opt.iter() {
		match step {
			Mutation::Random => {
				func.block_list.shuffle(&mut rng);
				func.child_list.shuffle(&mut rng);
				func.upval_list.shuffle(&mut rng);
				func.value_list.shuffle(&mut rng);
			}
			Mutation::Sorted => {
				func.block_list.sort_by_key(|v| v.label);
				func.child_list.sort_by_key(|v| Rc::clone(&v.0));
				func.upval_list.sort_by_key(|v| Rc::clone(&v.0));
				func.value_list.sort_by_key(|v| Rc::clone(&v.0));
			}
		}
	}
}

fn assemble_data(data: &[u8], opt: &[Mutation]) -> Result<()> {
	let mut func = from_bytes(data).expect("not valid RON");

	try_mutate(&mut func, opt);

	let proto = Proto::from(func);
	let binary = dump_lua_module(&proto)?;

	std::io::stdout().lock().write_all(&binary)
}

fn disassemble_data(data: &[u8], opt: &[Mutation]) -> Result<()> {
	let (trail, proto) = load_lua_module(data).expect("not valid Lua 5.4 bytecode");

	if !trail.is_empty() {
		panic!("trailing garbage in Lua file");
	}

	let mut func = Function::from(proto);

	try_mutate(&mut func, opt);

	let config = PrettyConfig::new();
	let ron = to_string_pretty(&func, config).expect("not convertible to RON");

	std::io::stdout().lock().write_all(ron.as_bytes())
}

fn list_help() {
	println!("usage: ldu [options]");
	println!("  -h | --help                show the help message");
	println!("  -a | --assemble [file]     assemble a RON file into bytecode");
	println!("  -d | --disassemble [file]  disassemble a bytecode file into RON");
	println!("  -r | --randomize           queue a randomization step");
	println!("  -s | --sort                queue a sorting step");
}

fn main() -> Result<()> {
	let mut iter = std::env::args().skip(1);
	let mut mutation = Vec::new();

	while let Some(val) = iter.next() {
		match val.as_str() {
			"-h" | "--help" => {
				list_help();
			}
			"-a" | "--assemble" => {
				let name = iter.next().expect("file name expected");
				let data = std::fs::read(name)?;

				assemble_data(&data, &mutation)?;
			}
			"-d" | "--disassemble" => {
				let name = iter.next().expect("file name expected");
				let data = std::fs::read(name)?;

				disassemble_data(&data, &mutation)?;
			}
			"-r" | "--randomize" => {
				mutation.push(Mutation::Random);
			}
			"-s" | "--sort" => {
				mutation.push(Mutation::Sorted);
			}
			opt => {
				panic!("unknown option `{}`", opt);
			}
		}
	}

	Ok(())
}
