use crate::cpu8080::*;
use bitmatch::bitmatch;
use std::*;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum RegId8 {
    A = 0x07,
    B = 0x00,
    C = 0x01,
    D = 0x02,
    E = 0x03,
    H = 0x04,
    L = 0x05,
    M = 0x06,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum RegId16 {
    BC = 0x00,
    DE = 0x01,
    HL = 0x10,
    SP = 0x11,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum CondId {
    NZ = 0x00,
    Z = 0x01,
    NC = 0x02,
    C = 0x03,
    PO = 0x04, // parity odd (pf == 0)
    PE = 0x05, // parity even (pf == 1)
    P = 0x06,  // plus (sf == 0)
    M = 0x07,  // minus (sf == 1)
}

#[allow(dead_code)]
const REG_ID8_MAP: [RegId8; 8] = [
    RegId8::B,
    RegId8::C,
    RegId8::D,
    RegId8::E,
    RegId8::H,
    RegId8::L,
    RegId8::M,
    RegId8::A,
];

#[allow(dead_code)]
const REG_ID16_MAP: [RegId16; 4] = [RegId16::BC, RegId16::DE, RegId16::HL, RegId16::SP];

#[allow(dead_code)]
const COND_ID_MAP: [CondId; 8] = [
    CondId::NZ,
    CondId::Z,
    CondId::NC,
    CondId::C,
    CondId::PO,
    CondId::PE,
    CondId::P,
    CondId::M,
];

impl fmt::Display for RegId8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegId8::A => write!(f, "A"),
            RegId8::B => write!(f, "B"),
            RegId8::C => write!(f, "C"),
            RegId8::D => write!(f, "D"),
            RegId8::E => write!(f, "E"),
            RegId8::H => write!(f, "H"),
            RegId8::L => write!(f, "L"),

            RegId8::M => write!(f, "(HL)"),
        }
    }
}

impl fmt::Display for RegId16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegId16::BC => write!(f, "BC"),
            RegId16::DE => write!(f, "DE"),
            RegId16::HL => write!(f, "HL"),
            RegId16::SP => write!(f, "SP"),
        }
    }
}

impl fmt::Display for CondId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CondId::NZ => write!(f, "NZ"),
            CondId::Z => write!(f, "Z"),
            CondId::NC => write!(f, "NC"),
            CondId::C => write!(f, "C"),
            CondId::PO => write!(f, "PO"),
            CondId::PE => write!(f, "PE"),
            CondId::P => write!(f, "P"),
            CondId::M => write!(f, "M"),
        }
    }
}

impl<MemMapT, IOBusT> Cpu8080<MemMapT, IOBusT>
where
    MemMapT: MemoryMap,
    IOBusT: IOBus,
{
    #[allow(dead_code)]
    fn get_reg8(&self, reg_id: RegId8) -> Reg8 {
        match reg_id {
            RegId8::A => self.state.reg_a,
            RegId8::B => self.state.reg_bc.h,
            RegId8::C => self.state.reg_bc.l,
            RegId8::D => self.state.reg_de.h,
            RegId8::E => self.state.reg_de.l,
            RegId8::H => self.state.reg_hl.h,
            RegId8::L => self.state.reg_hl.l,

            RegId8::M => panic!("{:?} it's not a register", reg_id),
        }
    }

    #[allow(dead_code)]
    fn set_reg8(&mut self, reg_id: RegId8, val: Reg8) {
        match reg_id {
            RegId8::A => self.state.reg_a = val,
            RegId8::B => self.state.reg_bc.h = val,
            RegId8::C => self.state.reg_bc.l = val,
            RegId8::D => self.state.reg_de.h = val,
            RegId8::E => self.state.reg_de.l = val,
            RegId8::H => self.state.reg_hl.h = val,
            RegId8::L => self.state.reg_hl.l = val,

            RegId8::M => panic!("{:?} it's not a register", reg_id),
        }
    }

    #[allow(dead_code)]
    fn get_reg16(&self, reg_id: RegId16) -> Reg16 {
        match reg_id {
            RegId16::BC => self.state.reg_bc.into(),
            RegId16::DE => self.state.reg_de.into(),
            RegId16::HL => self.state.reg_hl.into(),
            RegId16::SP => self.state.reg_sp,
        }
    }

    #[allow(dead_code)]
    fn set_reg16(&mut self, reg_id: RegId16, val: Reg16) {
        match reg_id {
            RegId16::BC => self.state.reg_bc = val.into(),
            RegId16::DE => self.state.reg_de = val.into(),
            RegId16::HL => self.state.reg_hl = val.into(),
            RegId16::SP => self.state.reg_sp = val,
        }
    }

    #[allow(dead_code)]
    fn check_condition(&self, cond_id: CondId) -> bool {
        return match cond_id {
            CondId::NZ => !self.state.flags.zf,
            CondId::Z => self.state.flags.zf,

            CondId::NC => !self.state.flags.cf,
            CondId::C => self.state.flags.cf,

            CondId::PO => !self.state.flags.pf,
            CondId::PE => self.state.flags.pf,

            CondId::P => !self.state.flags.sf,
            CondId::M => self.state.flags.sf,
        };
    }

    #[allow(dead_code)]
    fn push8(&mut self, b: u8) -> Result<(), MemoryMapError> {
        // decrease the stack pointer
        self.state.reg_sp -= 1;

        // save b in the stack
        return self.addr_space.write_b(self.state.reg_sp.into(), b);
    }

    #[allow(dead_code)]
    fn pop8(&mut self) -> Result<u8, MemoryMapError> {
        // read b from the stack
        let b = self.addr_space.read_b(self.state.reg_sp.into());

        // increase the stack pointer
        self.state.reg_sp += 1;

        return b;
    }

    #[allow(dead_code)]
    fn push16(&mut self, w: u16) -> Result<(), MemoryMapError> {
        // decrease the stack pointer
        self.state.reg_sp -= 2;

        // save b in the stack
        return self.addr_space.write_w(self.state.reg_sp.into(), w);
    }

    #[allow(dead_code)]
    fn pop16(&mut self) -> Result<u16, MemoryMapError> {
        // read b from the stack
        let w = self.addr_space.read_w(self.state.reg_sp.into());

        // increase the stack pointer
        self.state.reg_sp += 2;

        return w;
    }

    #[allow(dead_code)]
    #[bitmatch]
    pub fn fetch_and_execute(&mut self, execute: bool) -> Result<String, MemoryMapError> {
        let opcode: u8 = self.consume8()?;

        #[allow(unused_assignments)]
        let mut mnemonic: String = format!("{:#04x}", opcode);

        // useless loop just to be able to use the break keyword inside match
        for _useless in 0..1 {
            #[bitmatch]
            match opcode {
                // HLT
                "01110110" => {
                    // 0x76
                    mnemonic = format!("{:#04x}\tHLT\t\tUNIMPLEMENTED", opcode);
                    if !execute {
                        break;
                    }
                }

                // MOV D,S   01DDDSSS          -       Move register to register
                "01dddsss" => {
                    let dst_id: RegId8 = REG_ID8_MAP[d as usize];
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tMOV {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    // get the value of the src operand
                    let src_val: Reg8 = match src_id {
                        // src operand stored in memory
                        RegId8::M => {
                            let addr: u16 = self.state.reg_hl.into();
                            self.addr_space.read_b(addr)?
                        }

                        // src operand stored in register
                        _ => self.get_reg8(src_id),
                    };

                    // set the value of the dst operand
                    match dst_id {
                        // store dst operand in memory
                        RegId8::M => {
                            let addr: u16 = self.state.reg_hl.into();
                            self.addr_space.write_b(addr, src_val)?;
                        }

                        // store dst operand in register
                        _ => {
                            self.set_reg8(dst_id, src_val);
                        }
                    }
                }

                // MVI D,#   00DDD110 db       -       Move immediate to register
                "00ddd110" => {
                    let dst_id: RegId8 = REG_ID8_MAP[d as usize];
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tMVI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    // set the value of the dst operand
                    match dst_id {
                        // store dst operand in memory
                        RegId8::M => {
                            let addr: u16 = self.state.reg_hl.into();
                            self.addr_space.write_b(addr, src_val)?;
                        }

                        // store dst operand in register
                        _ => {
                            self.set_reg8(dst_id, src_val);
                        }
                    }
                }

                // LXI PP,#  00PP0001 lb hb    -       Load register pair immediate
                "00pp0001" => {
                    let dst_id: RegId16 = REG_ID16_MAP[p as usize];
                    let src_val: u16 = self.consume16()?;
                    mnemonic = format!("{:#04x}\tLXI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    self.set_reg16(dst_id, src_val);
                }

                // LDA a     00111010 lb hb    -       Load A from memory
                "00111010" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_addr: u16 = self.consume16()?;
                    mnemonic = format!("{:#04x}\tLDA {}, {}", opcode, dst_id, src_addr);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = self.addr_space.read_b(src_addr)?;
                    self.set_reg8(dst_id, src_val);
                }

                // STA a     00110010 lb hb    -       Store A to memory
                "00110010" => {
                    let dst_addr: u16 = self.consume16()?;
                    let src_id: RegId8 = RegId8::A;
                    mnemonic = format!("{:#04x}\tSTA {}, {}", opcode, dst_addr, src_id);
                    if !execute {
                        break;
                    }

                    let src_val = self.get_reg8(src_id);
                    self.addr_space.write_b(dst_addr, src_val)?;
                }

                // LHLD a    00101010 lb hb    -       Load H:L from memory
                "00101010" => {
                    let dst_id: RegId16 = RegId16::HL;
                    let src_addr: u16 = self.consume16()?;
                    mnemonic = format!("{:#04x}\tLHLD {}, {}", opcode, dst_id, src_addr);
                    if !execute {
                        break;
                    }

                    let src_val: u16 = self.addr_space.read_w(src_addr)?;
                    self.set_reg16(dst_id, src_val);
                }

                // SHLD a    00100010 lb hb    -       Store H:L to memory
                "00100010" => {
                    let dst_addr: u16 = self.consume16()?;
                    let src_id: RegId16 = RegId16::HL;
                    mnemonic = format!("{:#04x}\tSHLD {}, {}", opcode, dst_addr, src_id);
                    if !execute {
                        break;
                    }

                    let src_val = self.get_reg16(src_id);
                    self.addr_space.write_w(dst_addr, src_val)?;
                }

                // LDAX PP   00pp1010 *1       -       Load indirect through BC or DE
                "00pp1010" => {
                    let dst_id = RegId8::A;
                    let src_id: RegId16 = REG_ID16_MAP[p as usize];
                    match src_id {
                        RegId16::BC | RegId16::DE => {}
                        _ => panic!("Invalid src operand ({})", src_id),
                    }
                    mnemonic = format!("{:#04x}\tLDAX {}, ({})", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_addr: u16 = self.get_reg16(src_id);
                    let src_val: u8 = self.addr_space.read_b(src_addr)?;
                    self.set_reg8(dst_id, src_val);
                }

                // STAX PP   00PP0010 *1       -       Store indirect through BC or DE
                "00pp0010" => {
                    let dst_id: RegId16 = REG_ID16_MAP[p as usize];
                    let src_id = RegId8::A;
                    match dst_id {
                        RegId16::BC | RegId16::DE => {}
                        _ => panic!("Invalid src operand ({})", src_id),
                    }
                    mnemonic = format!("{:#04x}\tSTAX ({}), {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let dst_addr: u16 = self.get_reg16(dst_id);
                    let src_val: u8 = self.get_reg8(src_id);
                    self.addr_space.write_b(dst_addr, src_val)?;
                }

                // XCHG      11101011          -       Exchange DE and HL content
                "11101011" => {
                    let dst_id = RegId16::DE;
                    let src_id = RegId16::HL;
                    mnemonic = format!("{:#04x}\tXCHG {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let swap: u16 = self.get_reg16(dst_id);
                    self.set_reg16(dst_id, self.get_reg16(src_id));
                    self.set_reg16(src_id, swap);
                }

                // ADD S     10000SSS          ZSPCA   Add register to A
                "10000sss" => {
                    let dst_id = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tADD {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    let res =
                        self.add_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    self.set_reg8(dst_id, res);
                }

                // ADI #     11000110 db       ZSCPA   Add immediate to A
                "11000110" => {
                    let dst_id = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tADI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    let res =
                        self.add_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    self.set_reg8(dst_id, res);
                }

                // ADC S     10001SSS          ZSCPA   Add register to A with carry
                "10001sss" => {
                    let dst_id = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tADC {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    let res = self.add_set_flags8(
                        self.get_reg8(dst_id),
                        u8::wrapping_add(src_val, if self.state.flags.cf { 1 } else { 0 }),
                        flag_mask::ALL_FLAGS,
                    );

                    self.set_reg8(dst_id, res);
                }

                // ACI #     11001110 db       ZSCPA   Add immediate to A with carry
                "11001110" => {
                    let dst_id = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tACI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    let res = self.add_set_flags8(
                        self.get_reg8(dst_id),
                        u8::wrapping_add(src_val, if self.state.flags.cf { 1 } else { 0 }),
                        flag_mask::ALL_FLAGS,
                    );

                    self.set_reg8(dst_id, res);
                }

                // SUB S     10010SSS          ZSCPA   Subtract register from A
                "10010sss" => {
                    let dst_id = RegId8::A;
                    let src_id = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tSUB {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    let res =
                        self.sub_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    self.set_reg8(dst_id, res);
                }

                // SUI #     11010110 db       ZSCPA   Subtract immediate from A
                "11010110" => {
                    let dst_id = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tSUI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    let res: u8 =
                        self.sub_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    self.set_reg8(dst_id, res);
                }

                // SBB S     10011SSS          ZSCPA   Subtract register from A with borrow
                "10011sss" => {
                    let dst_id = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tSBB {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    let res = self.sub_set_flags8(
                        self.get_reg8(dst_id),
                        u8::wrapping_add(src_val, if self.state.flags.cf { 1 } else { 0 }),
                        flag_mask::ALL_FLAGS,
                    );

                    self.set_reg8(dst_id, res);
                }

                // SBI #     11011110 db       ZSCPA   Subtract immediate from A with borrow
                "11011110" => {
                    let dst_id = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tSBI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    let res = self.sub_set_flags8(
                        self.get_reg8(dst_id),
                        u8::wrapping_add(src_val, if self.state.flags.cf { 1 } else { 0 }),
                        flag_mask::ALL_FLAGS,
                    );

                    self.set_reg8(dst_id, res);
                }

                // INR D     00DDD100          ZSPA    Increment register
                "00ddd100" => {
                    let dst_id: RegId8 = REG_ID8_MAP[d as usize];
                    mnemonic = format!("{:#04x}\tINR {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = match dst_id {
                        RegId8::M => {
                            let dst_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(dst_addr)?
                        }
                        _ => self.get_reg8(dst_id),
                    };

                    let res: u8 = self.add_set_flags8(
                        dst_val,
                        1,
                        flag_mask::ALL_FLAGS & !flag_mask::CF, // all but CF
                    );

                    match dst_id {
                        RegId8::M => {
                            let dst_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.write_b(dst_addr, res)?
                        }
                        _ => self.set_reg8(dst_id, res),
                    };
                }

                // DCR D     00DDD101          ZSPA    Decrement register
                "00ddd101" => {
                    let dst_id: RegId8 = REG_ID8_MAP[d as usize];
                    mnemonic = format!("{:#04x}\tDCR {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = match dst_id {
                        RegId8::M => {
                            let dst_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(dst_addr)?
                        }
                        _ => self.get_reg8(dst_id),
                    };

                    let res: u8 = self.sub_set_flags8(
                        dst_val,
                        1,
                        flag_mask::ALL_FLAGS & !flag_mask::CF, // all but CF
                    );

                    match dst_id {
                        RegId8::M => {
                            let dst_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.write_b(dst_addr, res)?
                        }
                        _ => self.set_reg8(dst_id, res),
                    };
                }

                // INX PP    00PP0011          -       Increment register pair
                "00pp0011" => {
                    let dst_id: RegId16 = REG_ID16_MAP[p as usize];
                    mnemonic = format!("{:#04x}\tINX {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u16 = self.get_reg16(dst_id);

                    let res: u16 = self.add_set_flags16(dst_val, 1, flag_mask::NO_FLAGS);

                    self.set_reg16(dst_id, res);
                }

                // DCX PP    00PP1011          -       Decrement register pair
                "00pp1011" => {
                    let dst_id: RegId16 = REG_ID16_MAP[p as usize];
                    mnemonic = format!("{:#04x}\tDCX {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u16 = self.get_reg16(dst_id);

                    let res: u16 = self.sub_set_flags16(dst_val, 1, flag_mask::NO_FLAGS);

                    self.set_reg16(dst_id, res);
                }

                // DAD PP    00PP1001          C       Add register pair to HL (16 bit add)
                "00pp1001" => {
                    let dst_id = RegId16::HL;
                    let src_id: RegId16 = REG_ID16_MAP[p as usize];
                    mnemonic = format!("{:#04x}\tDAD {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let res = self.add_set_flags16(
                        self.get_reg16(dst_id),
                        self.get_reg16(src_id),
                        flag_mask::CF,
                    );

                    self.set_reg16(dst_id, res);
                }

                // DAA       00100111          ZSPCA   Decimal Adjust accumulator
                "00100111" => {
                    let dst_id = RegId8::A;
                    mnemonic = format!("{:#04x}\tDAA", opcode);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    let l: u8 = dst_val & 0x0fu8;

                    if l > 0 || self.state.flags.af {
                        let res: u8 =
                            self.add_set_flags8(self.get_reg8(dst_id), 6, flag_mask::ALL_FLAGS);
                        self.set_reg8(dst_id, res);
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    let mut h: u8 = (dst_val & 0xf0u8) >> 4;
                    if h > 9 || self.state.flags.cf {
                        h = self.add_set_flags8(h, 6, flag_mask::ALL_FLAGS);
                        h &= 0x0f;
                    }

                    let res: u8 = (h << 4) + (dst_val & 0x0f);
                    self.set_reg8(dst_id, res);
                }

                // ANA S     10100SSS          ZSCPA   AND register with A
                "10100sss" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tANA {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    let res: u8 = self.get_reg8(dst_id) & src_val;

                    // trick to update accordingly all flags
                    self.add_set_flags8(res, 0, flag_mask::ALL_FLAGS);

                    // this instruction resets the CF
                    self.state.flags.cf = false;

                    self.set_reg8(dst_id, res);
                }

                // ANI #     11100110 db       ZSPCA   AND immediate with A
                "11100110" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tANI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    let res: u8 = self.get_reg8(dst_id) & src_val;

                    // trick to update accordingly all flags
                    self.add_set_flags8(res, 0, flag_mask::ALL_FLAGS);

                    // this instruction resets CF and AF
                    self.state.flags.cf = false;
                    self.state.flags.af = false;

                    
                    self.set_reg8(dst_id, res);
                }

                // ORA S     10110SSS          ZSPCA   OR  register with A
                "10110sss" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tORA {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    // trick to update accordingly all flags
                    self.add_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    // this instruction resets CF and AF
                    self.state.flags.cf = false;
                    self.state.flags.af = false;

                    let res: u8 = self.get_reg8(dst_id) | src_val;
                    self.set_reg8(dst_id, res);
                }

                // ORI #     11110110          ZSPCA   OR  immediate with A
                "11110110" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tORI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    // trick to update accordingly all flags
                    self.add_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    // this instruction resets CF and AF
                    self.state.flags.cf = false;
                    self.state.flags.af = false;

                    let res: u8 = self.get_reg8(dst_id) | src_val;
                    self.set_reg8(dst_id, res);
                }

                // XRA S     10101SSS          ZSPCA   ExclusiveOR register with A
                "10101sss" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tXRA {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    let res: u8 = self.get_reg8(dst_id) ^ src_val;

                    // trick to update accordingly all flags
                    self.add_set_flags8(res, 0, flag_mask::ALL_FLAGS);

                    // this instruction resets CF and AF
                    self.state.flags.cf = false;
                    self.state.flags.af = false;

                    self.set_reg8(dst_id, res);
                }

                // XRI #     11101110 db       ZSPCA   ExclusiveOR immediate with A
                "11101110" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tXRI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    // trick to update accordingly all flags
                    self.add_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);

                    // this instruction resets CF and AF
                    self.state.flags.cf = false;
                    self.state.flags.af = false;

                    let res: u8 = self.get_reg8(dst_id) ^ src_val;
                    self.set_reg8(dst_id, res);
                }

                // CMP S     10111SSS          ZSPCA   Compare register with A
                "10111sss" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_id: RegId8 = REG_ID8_MAP[s as usize];
                    mnemonic = format!("{:#04x}\tCMP {}, {}", opcode, dst_id, src_id);
                    if !execute {
                        break;
                    }

                    let src_val: u8 = match src_id {
                        RegId8::M => {
                            let src_addr = self.get_reg16(RegId16::HL);
                            self.addr_space.read_b(src_addr)?
                        }
                        _ => self.get_reg8(src_id),
                    };

                    // trick to update accordingly all flags
                    self.sub_set_flags8(self.get_reg8(dst_id), src_val, flag_mask::ALL_FLAGS);
                }

                // CPI #     11111110          ZSPCA   Compare immediate with A
                "11111110" => {
                    let dst_id: RegId8 = RegId8::A;
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tCPI {}, ${}", opcode, dst_id, src_val);
                    if !execute {
                        break;
                    }

                    // trick to update accordingly all flags
                    let dst_val: u8 = self.get_reg8(dst_id);
                    self.sub_set_flags8(
                        dst_val,
                        src_val,
                        flag_mask::ALL_FLAGS
                    );
                }

                // RLC       00000111          C       Rotate A left
                "00000111" => {
                    let dst_id: RegId8 = RegId8::A;
                    mnemonic = format!("{:#04x}\tRLC {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    self.state.flags.cf = dst_val & 0x80u8 != 0;
                    let res = dst_val << 1;

                    self.set_reg8(dst_id, res);
                }

                // RRC       00001111          C       Rotate A right
                "00001111" => {
                    let dst_id: RegId8 = RegId8::A;
                    mnemonic = format!("{:#04x}\tRRC {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    self.state.flags.cf = dst_val & 0x01u8 != 0;
                    let res = dst_val >> 1;

                    self.set_reg8(dst_id, res);
                }

                // RAL       00010111          C       Rotate A left through carry
                "00010111" => {
                    let dst_id: RegId8 = RegId8::A;
                    mnemonic = format!("{:#04x}\tRAL {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    let old_carry = self.state.flags.cf;
                    self.state.flags.cf = dst_val & 0x80u8 != 0;
                    let res = dst_val << 1 + if old_carry { 1 } else { 0 };

                    self.set_reg8(dst_id, res);
                }

                // RAR       00011111          C       Rotate A right through carry
                "00011111" => {
                    let dst_id: RegId8 = RegId8::A;
                    mnemonic = format!("{:#04x}\tRAR {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    let old_carry = self.state.flags.cf;
                    self.state.flags.cf = dst_val & 0x01u8 != 0;
                    let res = dst_val >> 1 + if old_carry { 0x80u8 } else { 0 };

                    self.set_reg8(dst_id, res);
                }

                // CMA       00101111          -       Compliment A
                "00101111" => {
                    let dst_id: RegId8 = RegId8::A;
                    mnemonic = format!("{:#04x}\tCMA {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let dst_val: u8 = self.get_reg8(dst_id);
                    self.set_reg8(dst_id, !dst_val);
                }

                // CMC       00111111          C       Compliment Carry flag
                "00111111" => {
                    mnemonic = format!("{:#04x}\tCMC", opcode);
                    if !execute {
                        break;
                    }

                    self.state.flags.cf = !self.state.flags.cf;
                }

                // STC       00110111          C       Set Carry flag
                "00110111" => {
                    mnemonic = format!("{:#04x}\tSTC", opcode);
                    if !execute {
                        break;
                    }

                    self.state.flags.cf = true;
                }

                // JMP a     11000011 lb hb    -       Unconditional jump
                "11000011" => {
                    let src_val: u16 = self.consume16()?;
                    mnemonic = format!("{:#04x}\tJMP ${:#06x}", opcode, src_val);
                    if !execute {
                        break;
                    }

                    self.state.reg_pc = src_val;
                }

                // Jccc a    11CCC010 lb hb    -       Conditional jump
                "11ccc010" => {
                    let src_val: u16 = self.consume16()?;
                    let cond_id: CondId = COND_ID_MAP[c as usize];
                    mnemonic = format!("{:#04x}\tJ{} ${:#06x}", opcode, cond_id, src_val);
                    if !execute {
                        break;
                    }

                    if self.check_condition(cond_id) {
                        self.state.reg_pc = src_val;
                    }
                }

                // CALL a    11001101 lb hb    -       Unconditional subroutine call
                "11001101" => {
                    let src_val: u16 = self.consume16()?;
                    mnemonic = format!("{:#04x}\tCALL ${:#06x}", opcode, src_val);
                    if !execute {
                        break;
                    }

                    // save the program counter in the stack
                    self.push16(self.state.reg_pc)?;

                    // jump to the new addres
                    self.state.reg_pc = src_val.into();
                }

                // Cccc a    11CCC100 lb hb    -       Conditional subroutine call
                "11ccc100" => {
                    let src_val: u16 = self.consume16()?;
                    let cond_id: CondId = COND_ID_MAP[c as usize];
                    mnemonic = format!("{:#04x}\tCALL{} ${:#06x}", opcode, cond_id, src_val);
                    if !execute {
                        break;
                    }

                    if self.check_condition(cond_id) {
                        // save the program counter in the stack
                        self.push16(self.state.reg_pc)?;

                        // jump to the new addres
                        self.state.reg_pc = src_val.into();
                    }
                }

                // RET       11001001          -       Unconditional return from subroutine
                "11001001" => {
                    mnemonic = format!("{:#04x}\tRET", opcode);
                    if !execute {
                        break;
                    }

                    self.state.reg_pc = self.pop16()?;
                }

                // Rccc      11CCC000          -       Conditional return from subroutine
                "11ccc000" => {
                    let cond_id: CondId = COND_ID_MAP[c as usize];
                    mnemonic = format!("{:#04x}\tRET{}", opcode, cond_id);
                    if !execute {
                        break;
                    }

                    if self.check_condition(cond_id) {
                        self.state.reg_pc = self.pop16()?;
                    }
                }

                // RST n     11NNN111          -       Restart (Call n*8)
                "11nnn111" => {
                    let src_val = n as u16; // n * 8
                    mnemonic = format!("{:#04x}\tRST{}", opcode, src_val);
                    if !execute {
                        break;
                    }

                    // save the program counter in the stack
                    self.push16(self.state.reg_pc)?;

                    // jump to the new addres
                    self.state.reg_pc = src_val << 3;
                }

                // PCHL      11101001          -       Jump to address in H:L
                "11101001" => {
                    let src_id: RegId16 = RegId16::HL;
                    mnemonic = format!("{:#04x}\tPCHL {}", opcode, src_id);
                    if !execute {
                        break;
                    }

                    self.state.reg_pc = self.state.reg_hl.into();
                }

                // PUSH PP   11PP0101 *2       -       Push register pair on the stack
                "11pp0101" => {
                    let src_id: RegId16 = REG_ID16_MAP[p as usize];
                    let src_mne: String = match src_id {
                        RegId16::SP => "PSW".into(),
                        _ => format!("{}", src_id)
                    };

                    mnemonic = format!("{:#04x}\tPUSH {}", opcode, src_mne);
                    if !execute {
                        break;
                    }

                    let src_val: u16 = match src_id {
                        RegId16::SP => {
                            let psw_l: u8 = self.state.flags.into();
                            let psw_h: u8 = self.get_reg8(RegId8::A);
                            let psw: u16 = (psw_l as u16) + ((psw_h as u16) << 8);
                            psw
                        },
                        _ => self.get_reg16(src_id),
                    };                    
                    
                    self.push16(src_val)?;
                }

                // POP PP    11PP0001 *2       *2      Pop  register pair from the stack
                "11pp0001" => {
                    let dst_id: RegId16 = REG_ID16_MAP[p as usize];
                    let dst_mne: String = match dst_id {
                        RegId16::SP => "PSW".into(),
                        _ => format!("{}", dst_id),
                    };

                    mnemonic = format!("{:#04x}\tPOP {}", opcode, dst_mne);
                    if !execute {
                        break;
                    }

                    let w: u16 = self.pop16()?;
                    match dst_id {
                        RegId16::SP => {
                            self.state.flags = ((w & 0x00ff) as u8).into();
                            self.set_reg8(
                                RegId8::A,
                                (w >> 8) as u8
                            );
                        },
                        _ => { self.set_reg16(dst_id, w) },
                    };
                    
                }

                // XTHL      11100011          -       Swap H:L with top word on stack
                "11100011" => {
                    let dst_id: RegId16 = RegId16::HL;
                    mnemonic = format!("{:#04x}\tXTHL {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    let swap = self.get_reg16(dst_id);
                    let w: u16 = self.pop16()?;
                    self.set_reg16(dst_id, w);
                    self.push16(swap)?;
                }

                // SPHL      11111001          -       Set SP to content of H:L
                "11111001" => {
                    let dst_id: RegId16 = RegId16::HL;
                    mnemonic = format!("{:#04x}\tSPHL {}", opcode, dst_id);
                    if !execute {
                        break;
                    }

                    self.state.reg_sp = self.get_reg16(dst_id);
                }

                // IN p      11011011 pa       -       Read input port into A
                "11011011" => {
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tIN ${}", opcode, src_val);
                    if !execute {
                        break;
                    }

                    let dst_val = self.io_space.in_port(src_val);
                    self.state.reg_a = dst_val;
                }

                // OUT p     11010011 pa       -       Write A to output port
                "11010011" => {
                    let src_val: u8 = self.consume8()?;
                    mnemonic = format!("{:#04x}\tOUT ${}", opcode, src_val);
                    if !execute {
                        break;
                    }

                    let dst_val = self.state.reg_a;
                    self.io_space.out_port(src_val, dst_val);
                }

                // EI        11111011          -       Enable interrupts
                "11111011" => {
                    mnemonic = format!("{:#04x}\tEI", opcode);
                    if !execute {
                        break;
                    }

                    self.state.interrutpions_enabled = true;
                }

                // DI        11110011          -       Disable interrupts
                "11110011" => {
                    mnemonic = format!("{:#04x}\tDI", opcode);
                    if !execute {
                        break;
                    }

                    self.state.interrutpions_enabled = false;
                }

                // NOP       00000000          -       No operation
                "00000000" => {
                    mnemonic = format!("{:#04x}\tNOP", opcode);
                    if !execute {
                        break;
                    }
                }

                _ => {
                    // unimplemented_instruction!(opcode);
                    mnemonic = format!("{:#04x}\tINVALID INSTRUCTION", opcode);
                    if !execute {
                        break;
                    }
                }
            };
        }
        return Ok(mnemonic);
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    const CPUDIAG_BIN: &[u8] = include_bytes!("cpudiag.bin");

    #[test]
    fn test_all_instructions_decoding() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] }, TestIOBus {});

        for i in 0x00..=0xff {
            // avoid panics caused by not being able to grow / shrink the stack
            cpu.state.reg_sp = 1024;

            // set the next instruction
            cpu.addr_space.buff[cpu.state.reg_pc as usize] = i;

            println!("{}", cpu.fetch_and_execute(true).unwrap());
        }
    }    

    struct CpudiagIOBus {}
    impl IOBus for CpudiagIOBus {
        fn in_port(&mut self, port: u8) -> u8 {
            return 0;
        }

        fn out_port(&mut self, port: u8, data: u8) {
            
        }
    }

    #[test]
    fn test_cpudiag_full() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] }, CpudiagIOBus {});

        // add the bios
        cpu.addr_space.buff[0x0005] = 0xc9; // RET

        // add the program
        const FIRST_ADDR: usize = 0x0100;
        for i in 0..CPUDIAG_BIN.len() {
            cpu.addr_space.buff[i + FIRST_ADDR] = CPUDIAG_BIN[i];
        }

        let mut log_instructions = true;
        let mut program_output: String = "".into();
        cpu.state.reg_pc = FIRST_ADDR as u16;
        loop {
            let curr_state = cpu.state;
            let curr_pc = curr_state.reg_pc;

            if curr_pc == 0x0005 {
                match cpu.state.reg_bc.l {
                    2 => program_output = format!("{}{}", program_output, cpu.state.reg_de.l as char),
                    9 => {
                        let mut i: u16 = cpu.state.reg_de.into();
                        loop {
                            let c: char = cpu.addr_space.buff[i as usize] as char;
                            if c == '$' {
                                break;
                            }

                            i += 1;
                            program_output = format!("{}{}", program_output, c);
                        }
                    }                    
                    _ => {}
                };
            }

            // CPUER address
            if curr_pc == 0x0689 {
                log_instructions = false;
            }

            if curr_pc == 0x0000 {
                break;
            }

            let mne: String = cpu.fetch_and_execute(true).unwrap();
            if log_instructions {
                println!("{:#06x}|{:30}{}", curr_pc, mne, curr_state);
            }
        }

        println!("\n\nProgram output:\n{}\n", program_output);
    }
}
