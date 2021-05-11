#[macro_export]
macro_rules! ext_operand {
	($read:ident, $write:ident, $ret:ty, $range:expr) => {
		pub fn $read(self) -> $ret {
			use bit_field::BitField;

			self.inner.get_bits($range) as $ret
		}

		fn $write(mut self, value: $ret) -> Self {
			self.inner.set_bits($range, value as u32);
			self
		}
	};
}

#[macro_export]
macro_rules! ext_s_operand {
	($read:ident, $write:ident, $ret:ty, $range:expr) => {
		pub fn $read(self) -> $ret {
			static HALF: i32 = (1 << $range.end - $range.start) - 1 >> 1;

			(self.inner.get_bits($range) as i32 - HALF) as $ret
		}

		fn $write(mut self, value: $ret) -> Self {
			static HALF: i32 = (1 << $range.end - $range.start) - 1 >> 1;

			self.inner.set_bits($range, (value as i32 + HALF) as u32);
			self
		}
	};
}

pub mod loader;
pub mod types;
pub mod unique;
