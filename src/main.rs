use num::{FromPrimitive, ToPrimitive};
use num_derive::{FromPrimitive, ToPrimitive};

fn main() {
    println!("Hello, world!");
}

// r14: repeat index
// r15: stack pointer

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Register {
    nr: u8,
}

impl Register {
    fn new(nr: u8) -> Self {
        if nr > 15 {
            panic!("Register number too high");
        }
        Self { nr }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Immediate {
    value: u8,
}

impl Immediate {
    fn new(value: u8) -> Self {
        if value > 15 {
            panic!("Immediate value too high");
        }
        Self { value }
    }
}

// the first 2k of instructions gets turned into machine code.
// the rest, 6k, is memory
// the stack is not part of the memory, but is wiped between each run.

// we still have a lot of space left for other things that are not stored
// but mapped.

// neighbor maps - readable and writable during a single "evaluation"
// sensor/interaction memory - a depiction of the world around 32 bit
// registers, of which only the 16 low bits are used for addressing, the 16
// high bits exist and can be used arithmetic. They can be shifted into the low
// bits if they need to be written.

#[derive(Debug, ToPrimitive, FromPrimitive, Clone, Copy, PartialEq, Eq, Hash)]
enum Op {
    Block,
    LoadW,
    StoreW,
    LoadH,
    StoreH,
    LoadB,
    StoreB,
    LoadI,
    Copy,
    Add,
    Sub,
    Mul,
    Div,
    Sll,
    Srl,
    Sra,
    And,
    Or,
    Xor,
    IfEq,
    IfNe,
    IfGt,
    IfLt,
    IfGe,
    IfLe,
    Repeat,
    Push,
    Pop,
    Call,
    Noop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Instruction {
    /// Start/end of block. Bit pattern is in u8
    Block(u8),
    /// Load the full 32 bit value at address into register
    LoadW(Register, Register),
    /// Save the full 32 bit value of the register into the address
    StoreW(Register, Register),
    /// Load the 16 bit value of address of second register into the first
    LoadH(Register, Register),
    /// Save the 16 bit value of the first register into the address of the second
    StoreH(Register, Register),
    /// Load the byte value of address of second register into the first
    LoadB(Register, Register),
    /// Save the byte value of the first register into the address of the second
    /// The high byte is zeroed out.
    StoreB(Register, Register),
    /// Load an immediate value 0..15 into the register
    LoadI(Register, Immediate),
    /// Copy the value of the second register into the first
    Copy(Register, Register),
    /// Add the values of the second register to the first
    Add(Register, Register),
    /// Subtract the value of the second register from the first
    Sub(Register, Register),
    /// Multiply the values of the first and second register, store in first
    Mul(Register, Register),
    /// Divide the value of the first register by the second, store in first
    Div(Register, Register),
    /// Shift the value of the first register left by the value of the second
    Sll(Register, Register),
    /// Shift the value of the first register right by the value of the second
    Srl(Register, Register),
    /// Shift the value of the first register right by the value of the second, keeping the sign
    Sra(Register, Register),
    /// Bitwise and the values of the first and second register, store in first
    And(Register, Register),
    /// Bitwise or the values of the first and second register, store in first
    Or(Register, Register),
    /// Bitwise xor the values of the first and second register, store in first
    Xor(Register, Register),
    /// If r1 is equal to r2, jump to end of block
    IfEq(Register, Register),
    /// If r1 is not equal to r2, jump to end of block
    IfNe(Register, Register),
    // If r1 is greater than r2, jump to end of block
    IfGt(Register, Register),
    /// If r1 is less than r2, jump to end of block
    IfLt(Register, Register),
    /// If r1 is greater than or equal to r2, jump to end of block
    IfGe(Register, Register),
    /// If r1 is less than or equal to r2, jump to end of block
    IfLe(Register, Register),
    /// Repeat indicated by immediate - go back to start of block, set repeat register
    /// There is a total repeat budget, and if the repeat budget is exceeded, this
    /// instruction is a no-op. The repeat cost is calculated by multiplying all the
    /// repeat registers up to this register within the block. (including called blocks).
    /// If blocks are conditional, it's still counted as a repeat.
    Repeat(u8),
    /// Push value of the first register onto the stack indicated by second
    Push(Register, Register),
    /// Pop value into first register from stack indicated by second
    Pop(Register, Register),
    /// Call the block of instructions indicated by the bit pattern. The return
    /// address not placed on the stack as it shouldn't be overwritten (and is
    /// in general not preserved between runs); instead the call stack is
    /// maintained externally to main memory. the end block statement is a
    /// return. Can only call blocks below this one.
    Call(u8),
    /// Any instruction that can't be decoded is a Noop
    Noop(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct EncodedInstruction(u16);

impl EncodedInstruction {
    fn new_value(op: Op, value: u8) -> Self {
        let op_value = op.to_u8().unwrap();
        Self(u16::from_be_bytes([op_value, value]))
    }

    fn new_r1_r2(op: Op, r1: Register, r2: Register) -> Self {
        let value = (r1.nr << 4) | r2.nr;
        Self::new_value(op, value)
    }

    fn new_r1_immediate(op: Op, r1: Register, immediate: Immediate) -> Self {
        let value = (r1.nr << 4) | immediate.value;
        Self::new_value(op, value)
    }

    fn new_noop(value: u16) -> Self {
        if EncodedInstruction(value).op() != Op::Noop {
            panic!("Not a noop");
        }
        Self(value)
    }

    fn op(&self) -> Op {
        let op_value = (self.0 >> 8) as u8;
        let op = Op::from_u8(op_value);
        if let Some(op) = op {
            op
        } else {
            Op::Noop
        }
    }

    fn r1(&self) -> Register {
        // value is the lower 8 bits
        let value = (self.0 & 0b0000_0000_1111_1111) as u8;
        Register::new(value >> 4)
    }
    fn r2(&self) -> Register {
        Register::new((self.0 & 0b0000_0000_0000_1111) as u8)
    }

    fn value(&self) -> u8 {
        (self.0 & 0b0000_0000_1111_1111) as u8
    }

    fn immediate2(&self) -> Immediate {
        Immediate::new((self.0 & 0b0000_0000_0000_1111) as u8)
    }
}

impl From<u16> for EncodedInstruction {
    fn from(value: u16) -> Self {
        Self(value)
    }
}

impl From<EncodedInstruction> for u16 {
    fn from(encoded: EncodedInstruction) -> Self {
        encoded.0
    }
}

impl From<u16> for Instruction {
    fn from(value: u16) -> Self {
        EncodedInstruction(value).into()
    }
}

impl From<Instruction> for u16 {
    fn from(instruction: Instruction) -> Self {
        EncodedInstruction::from(instruction).into()
    }
}

impl From<EncodedInstruction> for Instruction {
    fn from(encoded: EncodedInstruction) -> Self {
        match encoded.op() {
            Op::Block => Instruction::Block(encoded.value()),
            Op::LoadW => Instruction::LoadW(encoded.r1(), encoded.r2()),
            Op::StoreW => Instruction::StoreW(encoded.r1(), encoded.r2()),
            Op::LoadH => Instruction::LoadH(encoded.r1(), encoded.r2()),
            Op::StoreH => Instruction::StoreH(encoded.r1(), encoded.r2()),
            Op::LoadB => Instruction::LoadB(encoded.r1(), encoded.r2()),
            Op::StoreB => Instruction::StoreB(encoded.r1(), encoded.r2()),
            Op::LoadI => Instruction::LoadI(encoded.r1(), encoded.immediate2()),
            Op::Copy => Instruction::Copy(encoded.r1(), encoded.r2()),
            Op::Add => Instruction::Add(encoded.r1(), encoded.r2()),
            Op::Sub => Instruction::Sub(encoded.r1(), encoded.r2()),
            Op::Mul => Instruction::Mul(encoded.r1(), encoded.r2()),
            Op::Div => Instruction::Div(encoded.r1(), encoded.r2()),
            Op::Sll => Instruction::Sll(encoded.r1(), encoded.r2()),
            Op::Srl => Instruction::Srl(encoded.r1(), encoded.r2()),
            Op::Sra => Instruction::Sra(encoded.r1(), encoded.r2()),
            Op::And => Instruction::And(encoded.r1(), encoded.r2()),
            Op::Or => Instruction::Or(encoded.r1(), encoded.r2()),
            Op::Xor => Instruction::Xor(encoded.r1(), encoded.r2()),
            Op::IfEq => Instruction::IfEq(encoded.r1(), encoded.r2()),
            Op::IfNe => Instruction::IfNe(encoded.r1(), encoded.r2()),
            Op::IfGt => Instruction::IfGt(encoded.r1(), encoded.r2()),
            Op::IfLt => Instruction::IfLt(encoded.r1(), encoded.r2()),
            Op::IfGe => Instruction::IfGe(encoded.r1(), encoded.r2()),
            Op::IfLe => Instruction::IfLe(encoded.r1(), encoded.r2()),
            Op::Repeat => Instruction::Repeat(encoded.value()),
            Op::Push => Instruction::Push(encoded.r1(), encoded.r2()),
            Op::Pop => Instruction::Pop(encoded.r1(), encoded.r2()),
            Op::Call => Instruction::Call(encoded.value()),
            Op::Noop => Instruction::Noop(encoded.0),
        }
    }
}

impl From<Instruction> for EncodedInstruction {
    fn from(instruction: Instruction) -> Self {
        match instruction {
            Instruction::Block(value) => EncodedInstruction::new_value(Op::Block, value),
            Instruction::LoadW(r1, r2) => EncodedInstruction::new_r1_r2(Op::LoadW, r1, r2),
            Instruction::StoreW(r1, r2) => EncodedInstruction::new_r1_r2(Op::StoreW, r1, r2),
            Instruction::LoadH(r1, r2) => EncodedInstruction::new_r1_r2(Op::LoadH, r1, r2),
            Instruction::StoreH(r1, r2) => EncodedInstruction::new_r1_r2(Op::StoreH, r1, r2),
            Instruction::LoadB(r1, r2) => EncodedInstruction::new_r1_r2(Op::LoadB, r1, r2),
            Instruction::StoreB(r1, r2) => EncodedInstruction::new_r1_r2(Op::StoreB, r1, r2),
            Instruction::LoadI(r1, immediate) => {
                EncodedInstruction::new_r1_immediate(Op::LoadI, r1, immediate)
            }
            Instruction::Copy(r1, r2) => EncodedInstruction::new_r1_r2(Op::Copy, r1, r2),
            Instruction::Add(r1, r2) => EncodedInstruction::new_r1_r2(Op::Add, r1, r2),
            Instruction::Sub(r1, r2) => EncodedInstruction::new_r1_r2(Op::Sub, r1, r2),
            Instruction::Mul(r1, r2) => EncodedInstruction::new_r1_r2(Op::Mul, r1, r2),
            Instruction::Div(r1, r2) => EncodedInstruction::new_r1_r2(Op::Div, r1, r2),
            Instruction::Sll(r1, r2) => EncodedInstruction::new_r1_r2(Op::Sll, r1, r2),
            Instruction::Srl(r1, r2) => EncodedInstruction::new_r1_r2(Op::Srl, r1, r2),
            Instruction::Sra(r1, r2) => EncodedInstruction::new_r1_r2(Op::Sra, r1, r2),
            Instruction::And(r1, r2) => EncodedInstruction::new_r1_r2(Op::And, r1, r2),
            Instruction::Or(r1, r2) => EncodedInstruction::new_r1_r2(Op::Or, r1, r2),
            Instruction::Xor(r1, r2) => EncodedInstruction::new_r1_r2(Op::Xor, r1, r2),
            Instruction::IfEq(r1, r2) => EncodedInstruction::new_r1_r2(Op::IfEq, r1, r2),
            Instruction::IfNe(r1, r2) => EncodedInstruction::new_r1_r2(Op::IfNe, r1, r2),
            Instruction::IfGt(r1, r2) => EncodedInstruction::new_r1_r2(Op::IfGt, r1, r2),
            Instruction::IfLt(r1, r2) => EncodedInstruction::new_r1_r2(Op::IfLt, r1, r2),
            Instruction::IfGe(r1, r2) => EncodedInstruction::new_r1_r2(Op::IfGe, r1, r2),
            Instruction::IfLe(r1, r2) => EncodedInstruction::new_r1_r2(Op::IfLe, r1, r2),
            Instruction::Repeat(value) => EncodedInstruction::new_value(Op::Repeat, value),
            Instruction::Push(r1, r2) => EncodedInstruction::new_r1_r2(Op::Push, r1, r2),
            Instruction::Pop(r1, r2) => EncodedInstruction::new_r1_r2(Op::Pop, r1, r2),
            Instruction::Call(value) => EncodedInstruction::new_value(Op::Call, value),
            Instruction::Noop(value) => EncodedInstruction::new_noop(value),
        }
    }
}

struct Instructions {
    instructions: Vec<Instruction>,
}

impl Instructions {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    fn push(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn iter(&self) -> std::slice::Iter<Instruction> {
        self.instructions.iter()
    }

    fn encode(&self) -> Vec<u8> {
        self.instructions
            .iter()
            .flat_map(|instruction| {
                let encoded_instruction: EncodedInstruction = (*instruction).into();
                encoded_instruction.0.to_be_bytes()
            })
            .collect()
    }
}

// turn an iterator of u8 into an iterator of instructions
struct DecodeInstructionIterator<'a> {
    iter: std::slice::Iter<'a, u8>,
}

impl<'a> Iterator for DecodeInstructionIterator<'a> {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        let op = match self.iter.next() {
            Some(op) => *op,
            // we can't find an op anymore, so we're done
            None => return None,
        };
        // now we try to find the value
        let value = match self.iter.next() {
            Some(value) => *value,
            // we can't find a value anymore, so we're done;
            // the instruction may be incompleted so we cut it off
            None => return None,
        };
        // now we can make a u16
        let encoded = u16::from_be_bytes([op, value]);
        // and we can make an encoded instruction
        let encoded_instruction: EncodedInstruction = encoded.into();
        // and we can make an instruction
        let instruction: Instruction = encoded_instruction.into();
        Some(instruction)
    }
}
