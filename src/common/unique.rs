use super::types::{Function, Named, Upvalue, Value};
use convert_case::{Case, Casing};
use std::{collections::HashMap, rc::Rc};

#[derive(Default)]
struct Unique<'a> {
	used: HashMap<Rc<str>, u32>,
	prefix: &'a str,
}

impl<'a> Unique<'a> {
	fn new(prefix: &'a str) -> Self {
		Self {
			used: HashMap::new(),
			prefix,
		}
	}

	fn find(&mut self, raw: &str) -> String {
		let name = raw.to_case(Case::Pascal).into();
		let index = self.used.entry(Rc::clone(&name)).or_default();

		*index += 1;

		format!("{}{}_{}", self.prefix, name, *index)
	}

	fn with<T, M>(&mut self, list: &[T], ext: M) -> Vec<Rc<str>>
	where
		M: Fn(&T) -> &str,
	{
		list.iter().map(ext).map(|v| self.find(v).into()).collect()
	}
}

pub fn name_value_list(list: Vec<Value>) -> Named<Value> {
	let name_list = Unique::new("v").with(&list, Value::as_str);

	name_list.into_iter().zip(list).collect()
}

pub fn name_upvalue_list(list: Vec<Upvalue>) -> Named<Upvalue> {
	let name_list = Unique::new("u").with(&list, |v| v.name.as_deref().unwrap_or("no_name"));

	name_list.into_iter().zip(list).collect()
}

pub fn name_child_list<T>(list: Vec<Function<T>>) -> Named<Function<T>> {
	let name_list = Unique::new("f").with(&list, |_| "function");

	name_list.into_iter().zip(list).collect()
}
