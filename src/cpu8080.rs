use std::*;

mod cpu8080_fetch;

type Reg8 = u8;
type Reg16 = u16;

#[derive(Copy, Clone, Default)]
struct FlagReg {
    zf: bool,
    sf: bool,
    pf: bool,
    cf: bool,
    ac: bool,
}

fn new_flag_reg() -> FlagReg { Default::default() }

#[derive(Copy, Clone, Default)]
struct Reg8Pair {
    h: Reg8,
    l: Reg8,
}

impl From<Reg16> for Reg8Pair {
    fn from(item: Reg16) -> Self {
        return Reg8Pair {
            h: ((0xff00u16 & item) >> 8) as u8,
            l: (0x00ff & item) as u8,
        };
    }
}

impl From<Reg8Pair> for Reg16 {
    fn from(item: Reg8Pair) -> Self {
        return ((item.h as Reg16) << 8) + (item.l as Reg16);
    }
}

impl ops::Add for Reg8Pair {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let self16: Reg16 = self.into();
        let other16: Reg16 = other.into();

        return (self16 + other16).into();
    }
}

impl ops::AddAssign for Reg8Pair {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

#[derive(Debug)]
enum MemoryMapError {}
impl fmt::Display for MemoryMapError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Memory Map Error")
    }
}

impl error::Error for MemoryMapError {}

trait MemoryMap {
    fn read_b(&self, addr: u16) -> Result<u8, MemoryMapError>;
    fn write_b(&mut self, addr: u16, b: u8) -> Result<(), MemoryMapError>;
}

struct Cpu8080<MemMapT>
where
    MemMapT: MemoryMap,
{
    // accumulator
    reg_a: Reg8,

    // general purpose registers
    reg_bc: Reg8Pair,
    reg_de: Reg8Pair,
    reg_hl: Reg8Pair,

    // program counter
    reg_pc: Reg16,

    // stack pointer
    reg_sp: Reg16,

    // flag registers
    flags: FlagReg,

    // addressable space (16 bit address)
    addr_space: MemMapT,
}

fn new_cpu8080<MemMapT>(mem_map: MemMapT) -> Cpu8080<MemMapT>
where
    MemMapT: MemoryMap,
{
    return Cpu8080 {
        reg_a: 0,
        reg_bc: 0.into(),
        reg_de: 0.into(),
        reg_hl: 0.into(),
        reg_pc: 0,
        reg_sp: 0,
        flags: new_flag_reg(),
        addr_space: mem_map,
    };
}

impl<MemMapT> Cpu8080<MemMapT>
where
    MemMapT: MemoryMap,
{
    pub fn reset(&mut self) {
        self.reg_pc = 0u16.into();
    }

    fn consume8(&mut self) -> Result<u8, MemoryMapError> {
        let b: u8 = self.addr_space.read_b(self.reg_pc.into())?;
        self.reg_pc += 1;
        return Ok(b);
    }

    fn consume16(&mut self) -> Result<u16, MemoryMapError> {
        return Ok((self.consume8()? as u16) + ((self.consume8()? as u16) << 8));
    }

    pub fn cycle(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reg8pair_from_u16() {
        let reg: Reg8Pair = Reg8Pair::from(0x8001u16);

        assert_eq!(reg.h, 0x80u8);
        assert_eq!(reg.l, 0x01u8);
    }

    #[test]
    fn test_reg8pair_into_u16() {
        let val: u16 = Reg8Pair {
            h: 0x80u8,
            l: 0x01u8,
        }
        .into();
        assert_eq!(val, 0x8001u16);
    }

    struct TestMemory {
        buff: [u8; 65536],
    }

    impl MemoryMap for TestMemory {
        fn read_b(&self, addr: u16) -> Result<u8, MemoryMapError> {
            let b: u8 = self.buff[addr as usize];
            return Ok(b);
        }

        fn write_b(&mut self, addr: u16, b: u8) -> Result<(), MemoryMapError> {
            self.buff[addr as usize] = b;
            return Ok(());
        }
    }

    #[test]
    fn test_memory_map() {
        const addr: u16 = 42u16;
        const b: u8 = 7u8;

        let mut mem = TestMemory { buff: [0; 65536] };

        mem.write_b(addr, b).unwrap();
        assert_eq!(mem.read_b(addr).unwrap(), b);
    }

    #[test]
    fn test_cpu_consume8() {
        const N_IT: u16 = 5;
        let mut mem = TestMemory { buff: [0; 65536] };
        for i in 0..N_IT {
            mem.buff[i as usize] = i as u8;
        }

        let mut cpu = new_cpu8080(mem);
        cpu.reg_pc = 0;

        for i in 0..N_IT {
            let b = cpu.consume8().unwrap();
            println!("PC: {}; b: {}", cpu.reg_pc, b);

            assert_eq!(b, i as u8);
            assert_eq!(cpu.reg_pc, i + 1 as u16);
        }
    }

    #[test]
    fn test_cpu_consume16() {
        const BL: u8 = 0x01u8;
        const BH: u8 = 0x80u8;
        const W: u16 = ((BH as u16) << 8) + (BL as u16);

        let mut mem = TestMemory { buff: [0; 65536] };
        mem.buff[0] = BL;
        mem.buff[1] = BH;

        let mut cpu = new_cpu8080(mem);
        cpu.reg_pc = 0;

        assert_eq!(cpu.consume16().unwrap(), W);
        assert_eq!(cpu.reg_pc, 2);
    }
}
