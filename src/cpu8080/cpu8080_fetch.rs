use crate::cpu8080::*;
use bitmatch::bitmatch;
use std::*;

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

#[derive(Debug, Clone, Copy)]
enum RegId16 {
    BC = 0x00,
    DE = 0x01,
    HL = 0x10,
    SP = 0x11,
}

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

const REG_ID16_MAP: [RegId16; 4] = [RegId16::BC, RegId16::DE, RegId16::HL, RegId16::SP];

impl<MemMapT> Cpu8080<MemMapT>
where
    MemMapT: MemoryMap,
{
    fn get_reg8(&self, reg_id: RegId8) -> Reg8 {
        match reg_id {
            RegId8::A => self.reg_a,
            RegId8::B => self.reg_bc.h,
            RegId8::C => self.reg_bc.l,
            RegId8::D => self.reg_de.h,
            RegId8::E => self.reg_de.l,
            RegId8::H => self.reg_hl.h,
            RegId8::L => self.reg_hl.l,

            RegId8::M => panic!("{:?} it's not a register", reg_id),
        }
    }

    fn set_reg8(&mut self, reg_id: RegId8, val: Reg8) {
        match reg_id {
            RegId8::A => self.reg_a = val,
            RegId8::B => self.reg_bc.h = val,
            RegId8::C => self.reg_bc.l = val,
            RegId8::D => self.reg_de.h = val,
            RegId8::E => self.reg_de.l = val,
            RegId8::H => self.reg_hl.h = val,
            RegId8::L => self.reg_hl.l = val,

            RegId8::M => panic!("{:?} it's not a register", reg_id),
        }
    }

    fn get_reg16(&self, reg_id: RegId16) -> Reg16 {
        match reg_id {
            RegId16::BC => self.reg_bc.into(),
            RegId16::DE => self.reg_de.into(),
            RegId16::HL => self.reg_hl.into(),
            RegId16::SP => self.reg_sp,
        }
    }

    fn set_reg16(&mut self, reg_id: RegId16, val: Reg16) {
        match reg_id {
            RegId16::BC => self.reg_bc = val.into(),
            RegId16::DE => self.reg_de = val.into(),
            RegId16::HL => self.reg_hl = val.into(),
            RegId16::SP => self.reg_sp = val,
        }
    }

    #[bitmatch]
    fn fetch_and_execute(&mut self) -> Result<(), MemoryMapError> {
        let opcode: u8 = self.consume8()?;
        let mut mnemonic: String = String::from("");

        #[bitmatch]
        match opcode {
            // HLT
            "01110110" => {
                // 0x76
                mnemonic = format!("{:#04x}\tHLT\t\tUNIMPLEMENTED", opcode);
            }

            // MOV D,S   01DDDSSS          -       Move register to register
            "01dddsss" => {
                let dst_id: RegId8 = REG_ID8_MAP[d as usize];
                let src_id: RegId8 = REG_ID8_MAP[s as usize];
                mnemonic = format!("{:#04x}\tMOV {}, {}", opcode, dst_id, src_id);

                // get the value of the src operand
                let src_val: Reg8 = match src_id {
                    // src operand stored in memory
                    RegId8::M => {                        
                        let addr: u16 = self.reg_hl.into();
                        self.addr_space.read_b(addr)?
                    },

                    // src operand stored in register
                    _ => self.get_reg8(src_id)
                };

                // set the value of the dst operand
                match dst_id {
                    // store dst operand in memory
                    RegId8::M => {
                        let addr: u16 = self.reg_hl.into();
                        self.addr_space.write_b(addr, src_val)?;
                    }

                    // store dst operand in register
                    _ => { self.set_reg8(dst_id, src_val); }
                }
            }

            // MVI D,#   00DDD110 db       -       Move immediate to register
            "00ddd110" => {
                let dst_id: RegId8 = REG_ID8_MAP[d as usize];
                let src_val: u8 = self.consume8()?;
                mnemonic = format!("{:#04x}\tMVI {}, ${}", opcode, dst_id, src_val);

                // set the value of the dst operand
                match dst_id {
                    // store dst operand in memory
                    RegId8::M => {
                        let addr: u16 = self.reg_hl.into();
                        self.addr_space.write_b(addr, src_val)?;
                    },

                    // store dst operand in register
                    _ => { self.set_reg8(dst_id, src_val); }
                }
            }

            // LXI PP,#  00PP0001 lb hb    -       Load register pair immediate
            "00pp0001" => {
                let dst_id: RegId16 = REG_ID16_MAP[p as usize];
                let src_val: u16 = self.consume16()?;
                mnemonic = format!("{:#04x}\tLXI {}, ${}", opcode, dst_id, src_val);

                self.set_reg16(dst_id, src_val);
            }

            // LDA a     00111010 lb hb    -       Load A from memory
            "00111010" => {
                let dst_id: RegId8 = RegId8::A;
                let src_addr: u16 = self.consume16()?;
                mnemonic = format!("{:#04x}\tLDA {}, {}", opcode, dst_id, src_addr);

                let src_val: u8 = self.addr_space.read_b(src_addr)?;
                self.set_reg8(dst_id, src_val);
            }

            // STA a     00110010 lb hb    -       Store A to memory
            "00110010" => {
                let dst_addr: u16 = self.consume16()?;
                let src_id: RegId8 = RegId8::A;
                mnemonic = format!("{:#04x}\tSTA {}, {}", opcode, dst_addr, src_id);

                let src_val = self.get_reg8(src_id);
                self.addr_space.write_b(dst_addr, src_val)?;
            }

            // LHLD a    00101010 lb hb    -       Load H:L from memory
            "00101010" => {
                let dst_id : RegId16 = RegId16::HL;
                let src_addr: u16 = self.consume16()?;
                mnemonic = format!("{:#04x}\tLHLD {}, {}", opcode, dst_id, src_addr);

                let src_val: u16 = self.addr_space.read_w(src_addr)?;
                self.set_reg16(dst_id, src_val);
            }

            // SHLD a    00100010 lb hb    -       Store H:L to memory
            "00100010" => {
                let dst_addr: u16 = self.consume16()?;
                let src_id: RegId16 = RegId16::HL;
                mnemonic = format!("{:#04x}\tSHLD {}, {}", opcode, dst_addr, src_id);

                let src_val = self.get_reg16(src_id);
                self.addr_space.write_w(dst_addr, src_val)?;
            }

            // LDAX PP   00pp1010 *1       -       Load indirect through BC or DE
            "00pp1010" => {
                let dst_id = RegId8::A;
                let src_id: RegId16 = REG_ID16_MAP[p as usize];
                match src_id {
                    RegId16::BC | RegId16::DE => {},
                    _ => panic!("Invalid src operand ({})", src_id),
                }
                mnemonic = format!("{:#04x}\tLDAX {}, ({})", opcode, dst_id, src_id);

                let src_addr: u16 = self.get_reg16(src_id);
                let src_val: u8 = self.addr_space.read_b(src_addr)?;
                self.set_reg8(dst_id, src_val);
            }

            // STAX PP   00PP0010 *1       -       Store indirect through BC or DE
            // XCHG      11101011          -       Exchange DE and HL content
            // ADD S     10000SSS          ZSPCA   Add register to A
            // ADI #     11000110 db       ZSCPA   Add immediate to A
            // ADC S     10001SSS          ZSCPA   Add register to A with carry
            // ACI #     11001110 db       ZSCPA   Add immediate to A with carry
            // SUB S     10010SSS          ZSCPA   Subtract register from A
            // SUI #     11010110 db       ZSCPA   Subtract immediate from A
            // SBB S     10011SSS          ZSCPA   Subtract register from A with borrow
            // SBI #     11011110 db       ZSCPA   Subtract immediate from A with borrow
            // INR D     00DDD100          ZSPA    Increment register
            // DCR D     00DDD101          ZSPA    Decrement register
            // INX PP    00PP0011          -       Increment register pair
            // DCX PP    00PP1011          -       Decrement register pair
            // DAD PP    00PP1001          C       Add register pair to HL (16 bit add)
            // DAA       00100111          ZSPCA   Decimal Adjust accumulator
            // ANA S     10100SSS          ZSCPA   AND register with A
            // ANI #     11100110 db       ZSPCA   AND immediate with A
            // ORA S     10110SSS          ZSPCA   OR  register with A
            // ORI #     11110110          ZSPCA   OR  immediate with A
            // XRA S     10101SSS          ZSPCA   ExclusiveOR register with A
            // XRI #     11101110 db       ZSPCA   ExclusiveOR immediate with A
            // CMP S     10111SSS          ZSPCA   Compare register with A
            // CPI #     11111110          ZSPCA   Compare immediate with A
            // RLC       00000111          C       Rotate A left
            // RRC       00001111          C       Rotate A right
            // RAL       00010111          C       Rotate A left through carry
            // RAR       00011111          C       Rotate A right through carry
            // CMA       00101111          -       Compliment A
            // CMC       00111111          C       Compliment Carry flag
            // STC       00110111          C       Set Carry flag
            // JMP a     11000011 lb hb    -       Unconditional jump
            // Jccc a    11CCC010 lb hb    -       Conditional jump
            // CALL a    11001101 lb hb    -       Unconditional subroutine call
            // Cccc a    11CCC100 lb hb    -       Conditional subroutine call
            // RET       11001001          -       Unconditional return from subroutine
            // Rccc      11CCC000          -       Conditional return from subroutine
            // RST n     11NNN111          -       Restart (Call n*8)
            // PCHL      11101001          -       Jump to address in H:L
            // PUSH PP   11PP0101 *2       -       Push register pair on the stack
            // POP PP    11PP0001 *2       *2      Pop  register pair from the stack
            // XTHL      11100011          -       Swap H:L with top word on stack
            // SPHL      11111001          -       Set SP to content of H:L
            // IN p      11011011 pa       -       Read input port into A
            // OUT p     11010011 pa       -       Write A to output port
            // EI        11111011          -       Enable interrupts
            // DI        11110011          -       Disable interrupts
            // HLT       01110110          -       Halt processor
            // NOP       00000000          -       No operation
            _ => {
                // unimplemented_instruction!(opcode);
                mnemonic = format!("{:#04x}\tUNIMPLEMENTED", opcode);
            }
        };

        println!("{}", mnemonic);
        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::*;

    #[test]
    fn test_all_instructions_decoding() {
        let mut cpu = Cpu8080::new(TestMemory { buff: [0; 65536] });

        for i in 0x00..=0xff {
            cpu.addr_space.buff[cpu.reg_pc as usize] = i;
            cpu.fetch_and_execute().unwrap();
        }
    }
}
