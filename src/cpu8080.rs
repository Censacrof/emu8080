use std::*;

mod fetch;

type Reg8 = u8;
type Reg16 = u16;

#[derive(Copy, Clone, Default, Debug, PartialEq)]
struct FlagReg {
    zf: bool,
    sf: bool,
    pf: bool,
    cf: bool,
    af: bool,
}

impl FlagReg {
    fn new() -> Self {
        return Default::default();
    }
}

impl From<FlagReg> for Reg8 {
    fn from(item: FlagReg) -> Self {
        let mut res: Self = 0x02;

        if item.cf { res |= flag_mask::CF }
        if item.pf { res |= flag_mask::PF }
        if item.af { res |= flag_mask::AF }
        if item.zf { res |= flag_mask::ZF }
        if item.sf { res |= flag_mask::SF }

        return res;
    }
}

impl From<Reg8> for FlagReg {
    fn from(item: Reg8) -> Self {
        return FlagReg {
            cf: item & flag_mask::CF != 0,
            pf: item & flag_mask::PF != 0,
            af: item & flag_mask::AF != 0,
            zf: item & flag_mask::ZF != 0,
            sf: item & flag_mask::SF != 0,
        };
    }
}

impl fmt::Display for FlagReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}{}{}",
            if self.cf { "C" } else { "c" },
            if self.pf { "P" } else { "p" },
            if self.af { "A" } else { "a" },
            if self.zf { "Z" } else { "z" },
            if self.sf { "S" } else { "s" },
        )
    }
}

#[derive(Copy, Clone, Default, Debug)]
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

    fn read_w(&self, addr: u16) -> Result<u16, MemoryMapError>;
    fn write_w(&mut self, addr: u16, b: u16) -> Result<(), MemoryMapError>;
}

#[derive(Debug, Copy, Clone, Default)]
struct Cpu8080State {
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

    interrutpions_enabled: bool,
}

impl fmt::Display for Cpu8080State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "A: {:#04x}; BC: {:#06x}; DE: {:#06x}; HL: {:#06x}; PC: {:#06x}; SP: {:#06x}; F: {}; IE: {};",
            self.reg_a,
            u16::from(self.reg_bc),
            u16::from(self.reg_de),
            u16::from(self.reg_hl),
            u16::from(self.reg_pc),
            u16::from(self.reg_sp),
            self.flags,
            self.interrutpions_enabled,
        )
    }
}

trait IOBus {
    fn in_port(&mut self, port: u8) -> u8;
    fn out_port(&mut self, port: u8, data: u8);
}

struct Cpu8080<MemMapT, IOBusT>
where
    MemMapT: MemoryMap,
    IOBusT: IOBus,
{
    state: Cpu8080State,

    // addressable space (16 bit address)
    addr_space: MemMapT,

    // I/O space (8 bit address)
    io_space: IOBusT,
}

mod flag_mask {
    pub const CF: u8 = 1;
    pub const PF: u8 = 4;
    pub const AF: u8 = 16;
    pub const ZF: u8 = 64;
    pub const SF: u8 = 128;    

    pub const ALL_FLAGS: u8 = 0xff;
    pub const NO_FLAGS: u8 = 0;
}

impl<MemMapT, IOBusT> Cpu8080<MemMapT, IOBusT>
where
    MemMapT: MemoryMap,
    IOBusT: IOBus,
{
    pub fn new(mem_map: MemMapT, io_bus: IOBusT) -> Self {
        return Self {
            state: Default::default(),
            addr_space: mem_map,
            io_space: io_bus,
        };
    }

    pub fn reset(&mut self) {
        self.state.reg_pc = 0u16.into();
    }

    fn consume8(&mut self) -> Result<u8, MemoryMapError> {
        let b: u8 = self.addr_space.read_b(self.state.reg_pc.into())?;
        self.state.reg_pc += 1;
        return Ok(b);
    }

    fn consume16(&mut self) -> Result<u16, MemoryMapError> {
        return Ok((self.consume8()? as u16) + ((self.consume8()? as u16) << 8));
    }

    pub fn cycle(&mut self) {}

    pub fn add_set_flags8(&mut self, dst: u8, src: u8, affected_flags: u8) -> u8 {
        let dst16 = dst as u16;
        let src16 = src as u16;
        let res16 = dst16 + src16;
        let res8 = res16 as u8;

        if affected_flags & flag_mask::ZF != 0 {
            self.state.flags.zf = res8 == 0;
        }

        if affected_flags & flag_mask::SF != 0 {
            self.state.flags.sf = res8 & 0x80u8 != 0;
        }

        if affected_flags & flag_mask::PF != 0 {
            let mut mod2sum = 0;
            for i in 0..8 {
                mod2sum = (mod2sum + (res8 >> i) & 0x01u8) % 2;
            }
            self.state.flags.pf = mod2sum == 0;
        }

        if affected_flags & flag_mask::CF != 0 {
            self.state.flags.cf = res16 & 0xff00u16 != 0;
        }

        if affected_flags & flag_mask::AF != 0 {
            let dst4 = dst & 0x0f;
            let src4 = src & 0x0f;
            self.state.flags.af = (dst4 + src4) & 0xf0 != 0;
        }

        return res8;
    }

    pub fn add_set_flags16(&mut self, dst: u16, src: u16, affected_flags: u8) -> u16 {
        let dst32 = dst as u32;
        let src32 = src as u32;
        let res32 = dst32 + src32;
        let res16 = res32 as u16;

        if affected_flags & flag_mask::ZF != 0 {
            self.state.flags.zf = res16 == 0;
        }

        if affected_flags & flag_mask::SF != 0 {
            self.state.flags.sf = res16 & 0x8000u16 != 0;
        }

        if affected_flags & flag_mask::PF != 0 {
            let mut mod2sum = 0;
            for i in 0..16 {
                mod2sum = (mod2sum + (res16 >> i) & 0x0001u16) % 2;
            }
            self.state.flags.pf = mod2sum == 0;
        }

        if affected_flags & flag_mask::CF != 0 {
            self.state.flags.cf = res32 & 0xffff0000u32 != 0;
        }

        if affected_flags & flag_mask::AF != 0 {
            let dst4 = dst & 0x0f;
            let src4 = src & 0x0f;
            self.state.flags.af = (dst4 + src4) & 0xf0 != 0;
        }

        return res16;
    }

    fn sub_set_flags8(&mut self, dst: u8, src: u8, affected_flags: u8) -> u8 {
        let compl_src: u8 = u8::wrapping_add(!src, 1u8);
        let res = self.add_set_flags8(dst, compl_src, affected_flags);
        self.state.flags.cf = dst < src;
        return res;
    }

    fn sub_set_flags16(&mut self, dst: u16, src: u16, affected_flags: u8) -> u16 {
        let compl_src: u16 = u16::wrapping_add(!src, 1u16);
        let res = self.add_set_flags16(dst, compl_src, affected_flags);
        self.state.flags.cf = dst < src;
        return res;
    }
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

    pub struct TestMemory {
        pub buff: [u8; 65536],
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

        fn read_w(&self, addr: u16) -> Result<u16, MemoryMapError> {
            let addr_i = addr as usize;
            let w: u16 = (self.buff[addr_i] as u16) + ((self.buff[addr_i + 1] as u16) << 8);
            return Ok(w);
        }

        fn write_w(&mut self, addr: u16, w: u16) -> Result<(), MemoryMapError> {
            let addr_i = addr as usize;
            self.buff[addr_i] = (w & 0x00ff) as u8;
            self.buff[addr_i + 1] = ((w & 0xff00) >> 8) as u8;
            return Ok(());
        }
    }

    pub struct TestIOBus {}
    impl IOBus for TestIOBus {
        fn in_port(&mut self, port: u8) -> u8 {
            return 0;
        }
        fn out_port(&mut self, port: u8, data: u8) {}
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

        let mut cpu = Cpu8080::new(mem, TestIOBus {});
        cpu.state.reg_pc = 0;

        for i in 0..N_IT {
            let b = cpu.consume8().unwrap();
            println!("PC: {}; b: {}", cpu.state.reg_pc, b);

            assert_eq!(b, i as u8);
            assert_eq!(cpu.state.reg_pc, i + 1 as u16);
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

        let mut cpu = Cpu8080::new(mem, TestIOBus {});
        cpu.state.reg_pc = 0;

        assert_eq!(cpu.consume16().unwrap(), W);
        assert_eq!(cpu.state.reg_pc, 2);
    }

    #[test]
    fn test_cpu_add_set_flags8() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] }, TestIOBus {});

        // ------------
        let a = 0xffu8;
        let b = 0x01u8;
        let res = cpu.add_set_flags8(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#04x} + {:#04x} = {:#04x}; flags: {:?}",
            a, b, res, cpu.state.flags
        );
        assert_eq!(res, 0x00u8);
        assert_eq!(
            cpu.state.flags,
            FlagReg {
                zf: true,
                sf: false,
                pf: true,
                cf: true,
                af: true
            }
        );

        // ------------
        let a = 0x8fu8;
        let b = 0x01u8;
        let res = cpu.add_set_flags8(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#04x} + {:#04x} = {:#04x}; flags: {:?}",
            a, b, res, cpu.state.flags
        );
        assert_eq!(res, 0x90u8);
        assert_eq!(
            cpu.state.flags,
            FlagReg {
                zf: false,
                sf: true,
                pf: true,
                cf: false,
                af: true
            }
        );

        // ------------
        let a = 0x05u8;
        let b = 0x03u8;
        let res = cpu.add_set_flags8(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#04x} + {:#04x} = {:#04x}; flags: {:?}",
            a, b, res, cpu.state.flags
        );
        assert_eq!(res, 0x08u8);
        assert_eq!(
            cpu.state.flags,
            FlagReg {
                zf: false,
                sf: false,
                pf: false,
                cf: false,
                af: false
            }
        );
    }

    #[test]
    fn test_cpu_add_set_flags16() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] }, TestIOBus {});

        // ---------------
        let a = 0xffffu16;
        let b = 0x0001u16;
        let res = cpu.add_set_flags16(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#06x} + {:#06x} = {:#06x}; flags: {:?}",
            a, b, res, cpu.state.flags
        );
        assert_eq!(res, 0x0000u16);
        assert_eq!(
            cpu.state.flags,
            FlagReg {
                zf: true,
                sf: false,
                pf: true,
                cf: true,
                af: true
            }
        );

        // ---------------
        let a = 0x80ffu16;
        let b = 0x0001u16;
        let res = cpu.add_set_flags16(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#06x} + {:#06x} = {:#06x}; flags: {:?}",
            a, b, res, cpu.state.flags
        );
        assert_eq!(res, 0x8100u16);
        assert_eq!(
            cpu.state.flags,
            FlagReg {
                zf: false,
                sf: true,
                pf: true,
                cf: false,
                af: true
            }
        );

        // ---------------
        let a = 0x0005u16;
        let b = 0x0003u16;
        let res = cpu.add_set_flags16(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#06x} + {:#06x} = {:#06x}; flags: {:?}",
            a, b, res, cpu.state.flags
        );
        assert_eq!(res, 0x08u16);
        assert_eq!(
            cpu.state.flags,
            FlagReg {
                zf: false,
                sf: false,
                pf: false,
                cf: false,
                af: false
            }
        );
    }

    #[test]
    fn test_cpu_sub_set_flags8() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] }, TestIOBus {});

        // ------------
        let a = 0x80u8;
        let b = 0x01u8;
        let res = cpu.sub_set_flags8(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#04x} - {:#04x} = {:#04x}; borrow_out (CF): {}",
            a, b, res, cpu.state.flags.cf
        );
        assert_eq!(res, 0x7fu8);
        assert_eq!(cpu.state.flags.cf, false);

        // ------------
        let a = 0x07u8;
        let b = 0xb5u8;
        let res = cpu.sub_set_flags8(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#04x} - {:#04x} = {:#04x}; borrow_out (CF): {}",
            a, b, res, cpu.state.flags.cf
        );
        assert_eq!(res, 0x52u8);
        assert_eq!(cpu.state.flags.cf, true);
    }

    #[test]
    fn test_cpu_sub_set_flags16() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] }, TestIOBus {});

        // ------------
        let a = 0x0080u16;
        let b = 0x0001u16;
        let res = cpu.sub_set_flags16(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#06x} - {:#06x} = {:#06x}; borrow_out (CF): {}",
            a, b, res, cpu.state.flags.cf
        );
        assert_eq!(res, 0x007fu16);
        assert_eq!(cpu.state.flags.cf, false);

        // ------------
        let a = 0x0007u16;
        let b = 0x00b5u16;
        let res = cpu.sub_set_flags16(a, b, flag_mask::ALL_FLAGS);
        println!(
            "{:#04x} - {:#04x} = {:#04x}; borrow_out (CF): {}",
            a, b, res, cpu.state.flags.cf
        );
        assert_eq!(res, 0xff52u16);
        assert_eq!(cpu.state.flags.cf, true);
    }
}
