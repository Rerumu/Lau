use crate::common::types::Res;
use nom::{
	bytes::complete::take_while_m_n,
	combinator::{map, verify},
	number::complete::u8,
};
use std::{
	convert::TryFrom,
	io::{Result, Write},
	iter::once,
	mem::size_of,
};

pub type Unsigned = u64;

const TAIL_LEN: usize = size_of::<Unsigned>() * 8 / 7 - 1;
const UNSG_LEN: usize = TAIL_LEN + 1;

pub fn dump_unsigned(mut val: Unsigned, w: &mut dyn Write) -> Result<()> {
	let mut result = [0x80; UNSG_LEN];

	for v in result.iter_mut().rev() {
		*v = u8::try_from(val & 0x7F).unwrap();
		val >>= 7;

		if val == 0 {
			break;
		}
	}

	let start = result.iter().position(|v| v & 0x80 == 0).unwrap();

	result[UNSG_LEN - 1] |= 0x80;

	w.write_all(&result[start..])
}

pub fn load_unsigned(input: &[u8]) -> Res<Unsigned> {
	let (input, tail) = take_while_m_n(0, TAIL_LEN, |v| v & 0x80 == 0)(input)?;
	let (input, head) = map(verify(u8, |v| v & 0x80 != 0), |v| v & 0x7F)(input)?;
	let result = tail
		.iter()
		.cloned()
		.chain(once(head))
		.fold(0, |acc, x| acc << 7 | Unsigned::from(x));

	Ok((input, result))
}
