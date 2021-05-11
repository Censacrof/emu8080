type Reg8 = u8;

struct Reg16 {
    h: Reg8,
    l: Reg8,
}

impl From<u16> for Reg16 {
    fn from(item: u16) -> Self {
        return Reg16 { 
            h: ((0xff00u16 & item) >> 8) as u8,
            l: (0x00ff & item) as u8, 
        };
    }
}

impl From<Reg16> for u16 {
    fn from(item: Reg16) -> Self {
        return ((item.h as u16) << 8) + (item.l as u16);
    }
}

struct Cpu8080 {
    // accumulator
    reg_a: Reg8,

    // general purpose registers
    reg_bc: Reg16,
    reg_de: Reg16,
    reg_hl: Reg16,

    // flag register
    reg_f: Reg8,

    // program counter
    reg_pc: Reg16,

    // stack pointer
    reg_sp: Reg16,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reg16_from_u16() {
        let reg: Reg16 = Reg16::from(0x8001u16);

        assert_eq!(reg.h, 0x80u8);
        assert_eq!(reg.l, 0x01u8);
    }

    #[test]
    fn test_reg16_into_u16() {
        let val : u16 = Reg16 { h: 0x80u8, l: 0x01u8 }.into();
        assert_eq!(val, 0x8001u16);
    }
}