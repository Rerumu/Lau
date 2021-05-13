use super::types::{Res, ENDIANNESS};
use nom::{
	combinator::verify,
	number::{
		complete::{self, u8},
		Endianness,
	},
};
use std::{
	io::{Result, Write},
	mem::size_of,
};

pub fn verify_size_of<T>(input: &[u8]) -> Res<u8> {
	verify(u8, |&v| size_of::<T>() == usize::from(v))(input)
}

macro_rules! impl_serde {
	($t:ty, $func:expr) => {
		impl Serde for $t {
			fn ser(self, w: &mut dyn Write) -> Result<()> {
				match ENDIANNESS {
					Endianness::Big => w.write_all(&self.to_be_bytes()),
					Endianness::Little => w.write_all(&self.to_le_bytes()),
					Endianness::Native => w.write_all(&self.to_ne_bytes()),
				}
			}

			fn deser(input: &[u8]) -> Res<Self> {
				$func(ENDIANNESS)(input)
			}
		}
	};
}

pub trait Serde
where
	Self: Sized,
{
	fn ser(self, w: &mut dyn Write) -> Result<()>;
	fn deser(input: &[u8]) -> Res<Self>;
}

impl Serde for i8 {
	fn ser(self, w: &mut dyn Write) -> Result<()> {
		w.write_all(&self.to_le_bytes())
	}

	fn deser(input: &[u8]) -> Res<Self> {
		complete::i8(input)
	}
}

impl Serde for u8 {
	fn ser(self, w: &mut dyn Write) -> Result<()> {
		w.write_all(&self.to_le_bytes())
	}

	fn deser(input: &[u8]) -> Res<Self> {
		complete::u8(input)
	}
}

impl_serde!(f32, complete::f32);
impl_serde!(f64, complete::f64);
impl_serde!(i128, complete::i128);
impl_serde!(i16, complete::i16);
impl_serde!(i32, complete::i32);
impl_serde!(i64, complete::i64);
impl_serde!(u128, complete::u128);
impl_serde!(u16, complete::u16);
impl_serde!(u32, complete::u32);
impl_serde!(u64, complete::u64);
