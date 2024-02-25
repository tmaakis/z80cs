namespace z80cs;
public class CPU(ushort memsize)
{
	public bool halt = false;
	public byte[] Mem { get; set; } = new byte[memsize];
	public ushort[] IndexRegisters = new ushort[2]; // easy switching between IX and IY for the dd and fd opcode prefixes
	public ushort IX { get { return IndexRegisters[0]; } set { IndexRegisters[0] = value; } }
	public ushort IY { get { return IndexRegisters[1]; } set { IndexRegisters[1] = value; } }
	public ushort SP = 0, PC = 0;
	public byte I = 0, R = 0;
	public byte[] Registers = new byte[8];
	public byte[] AltRegisters = new byte[8];
	public byte A { get { return Registers[0]; } set { Registers[0] = value; } }
	public byte F { get { return Registers[1]; } set { Registers[1] = value; } }
	public byte B { get { return Registers[2]; } set { Registers[2] = value; } }
	public byte C { get { return Registers[3]; } set { Registers[3] = value; } }
	public byte D { get { return Registers[4]; } set { Registers[4] = value; } }
	public byte E { get { return Registers[5]; } set { Registers[5] = value; } }
	public byte H { get { return Registers[6]; } set { Registers[6] = value; } }
	public byte L { get { return Registers[7]; } set { Registers[7] = value; } }
	public ushort AF { get { return (ushort)((A << 8) | F); } set { A = (byte)(value >> 8); F = (byte)(value & 0xff); } }
	public ushort BC { get { return (ushort)((B << 8) | C); } set { B = (byte)(value >> 8); C = (byte)(value & 0xff); } }
	public ushort DE { get { return (ushort)((D << 8) | E); } set { D = (byte)(value >> 8); E = (byte)(value & 0xff); } }
	public ushort HL { get { return (ushort)((H << 8) | L); } set { H = (byte)(value >> 8); L = (byte)(value & 0xff); } }


	/* virtual public byte PortIN(CPU z80, byte nbyte)
	{
		Console.WriteLine("IN called");
		return A;
	}
	virtual public void PortOUT(CPU z80, byte nbyte)
	{
		Console.WriteLine("OUT called");
	} */
	virtual public byte PortIN(CPU z80, byte port)
	{
		z80.A = 0x00;
		return z80.A;
	}
	virtual public void PortOUT(CPU z80, byte port)
	{
		if (port == 0)
		{
			z80.halt = true;
		}
		else if (port == 1)
		{
			if (z80.C == 9)
			{
				ushort addr = (ushort)((z80.D << 8) | z80.E);
				do
				{
					Console.Write((char)z80.Mem[addr++]);
				} while ((char)z80.Mem[addr] != '$');
			}
			else if (z80.C == 2)
			{
				Console.Write((char)z80.E);
			}
		}
	}
	public byte ICYC;
	public bool interrupt = false;
	private static bool Parity(ushort ans) // left shift by 1 and add 0 if 0 and 1 if 1, then keep left shifting 1
	{
		byte oddcount = 0;
		for (byte i = 0; i < 8; i++)
		{
			oddcount += (byte)((ans >> i) & 1);
		}
		return (oddcount & 1) == 0;
	}
	// byte/ushort flags since "setting" them sets S, Z, P and CY
	public ushort S { get { return (byte)((F & 128)>>7); } set { F ^= (byte)((-(value & 128) ^ F) & 128); } }
	public ushort Z { get { return (ushort)((F & 64)>>6); } set { F ^= (byte)((-Convert.ToByte(value == 0) ^ F) & 64); } }
	public byte P { get { return (byte)((F & 4)>>2); } set { F ^= (byte)((-Convert.ToByte(Parity(value)) ^ F) & 4); } }
	public ushort V { get { return (byte)(F & 4); } set { F ^= (byte)((-Convert.ToByte(~value + 1 > 0xff) ^ F) & 4); } } // same bit as parity, only inc, dec, and the sub/add instructions
	// flags below need setting manually
	public bool CY { get { return (F & 1) == 1; } set { F ^= (byte)((-Convert.ToByte(value) ^ F) & 1); } }
	public bool HY { get { return (F & 16) == 1; } set { F ^= (byte)((-Convert.ToByte(value) ^ F) & 16); } } // not done
	public byte N { get { return (byte)((F & 0b10)>>1); } set { F ^= (byte)((-value ^ F) & 0b10); } } // add = false, sub = true
	// these are special bits
	public byte F3 { get { return (byte)((F & 8)>>3); } set { F ^= (byte)((-value ^ F) & 8); } }
	public byte F5 { get { return (byte)((F & 32)>>5); } set { F ^= (byte)((-value ^ F) & 32); } }
	public void MemWrite(ushort adr, byte val) // provide a safe memory writing function
	{
		if (adr < adr) // program crashes if it tries to write to somewhere that doesn't exist anyway
		{
			Console.WriteLine($"Warning: Tried to write to {adr:x4}, which is in ROM");
		}
		Mem[adr] = val;
	}
	public void MemWrite(ushort adr, byte loval, byte hival)
	{
		MemWrite(adr, loval);
		MemWrite((ushort)(adr + 1), hival);
	}
	public void MemWrite(ushort adr, ushort val)
	{
		MemWrite((ushort)(adr + 1), (byte)(val >> 8));
		MemWrite(adr, (byte)val);
	}
}

class Run
{
	private static int opbytes = 1;
	private static bool Carry(ushort reg1, ushort reg2, bool cy, byte bitno = 8)
	{
		ushort result = (ushort)(reg1 + reg2 + Convert.ToByte(cy));
		ushort carry = (ushort)(result ^ reg1 ^ reg2);
		return Convert.ToBoolean(carry & (1 << bitno));
	}
	private static byte SetBit(byte reg, byte bitpos)
	{
		return (byte)(reg | (1 << bitpos));
	}
	private static byte GetBit(byte reg, byte bitpos)
	{
		return (byte)((reg >> bitpos) & 1);
	}
	private static CPU ExAF(CPU z80)
	{
		(z80.AltRegisters[0], z80.A) = (z80.A, z80.AltRegisters[0]);
		(z80.AltRegisters[1], z80.F) = (z80.F, z80.AltRegisters[1]);
		z80.ICYC = 4;
		return z80;
	}

	private static CPU DEC(CPU z80, ushort result)
	{
		z80.N = 1;
		z80.S = result;
		z80.Z = result;
		z80.V = result;// == 128;
		z80.HY = (result & 0xe) == 0;
		return z80;
	}

	private static CPU INC(CPU z80, ushort result)
	{
		z80.N = 0;
		z80.S = result;
		z80.Z = result;
		z80.V = result;// == 127;
		z80.HY = (result & 0xf) == 0;
		return z80;
	}

	private static CPU ADDHL(CPU z80, ushort regadd)
	{
		uint result = (ushort)(z80.HL + regadd);
		z80.N = 0;
		z80.HY = ((z80.HL & 2048) + (regadd & 2048)) > 2048;
		z80.CY = (result - 0xff00) > 0xff;
		z80.HL = (ushort)result;
		z80.ICYC = 11;
		return z80;
	}

	private static CPU ADDI(CPU z80, ushort regadd, byte irptr)
	{
		uint result = (ushort)(z80.IndexRegisters[irptr] + regadd);
		z80.N = 0;
		z80.HY = ((z80.HL & 2048) + (regadd & 2048)) > 2048;
		z80.CY = (result - 0xff00) > 0xff;
		z80.IndexRegisters[irptr] = (ushort)result;
		z80.ICYC = 15;
		return z80;
	}

	private static CPU ADD(CPU z80, ushort regadd, bool carry = false, bool setA = true)
	{
		ushort result = (ushort)(z80.A + regadd + Convert.ToByte(carry));
		z80.N = 0;
		z80.S = result;
		z80.Z = result;
		z80.V = result;
		z80.HY = Carry(z80.A, regadd, carry, 4);
		z80.CY = Carry(z80.A, regadd, carry);
		z80.F3 = 0;//GetBit((byte)result, 3);
		z80.F5 = 0;//GetBit((byte)result, 5);
		if (setA) z80.A = (byte)result;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU SUB(CPU z80, ushort regadd, bool carry = false, bool setA = true)
	{
		ADD(z80, (ushort)(~regadd), !carry, setA);
		z80.CY = !z80.CY;
		z80.HY = !z80.HY;
		z80.N = 1;
		return z80;
	}

	private static CPU Logic(CPU z80, byte result)
	{
		z80.CY = false;
		z80.N = 0;
		z80.S = result;
		z80.Z = result;
		z80.P = result;
		z80.HY = false;
		z80.A = result;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU RLC(CPU z80, byte val)
	{
		byte leftbit = (byte)(z80.Registers[val] >> 7);
		z80.CY = Convert.ToBoolean(leftbit);
		z80.Registers[val] = (byte)((byte)(z80.Registers[val] << 1) | leftbit);
		z80.N = 0;
		z80.HY = false;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU RRC(CPU z80, byte val)
	{
		byte rightbit = (byte)(z80.Registers[val] << 7);
		z80.CY = Convert.ToBoolean(rightbit >> 7);
		z80.Registers[val] = (byte)((byte)(z80.Registers[val] >> 1) | rightbit);
		z80.N = 0;
		z80.HY = false;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU RL(CPU z80, byte val)
	{
		byte leftbit = (byte)(z80.Registers[val] >> 7);
		z80.Registers[val] = (byte)((byte)(z80.Registers[val] << 1) | Convert.ToByte(z80.CY));
		z80.CY = Convert.ToBoolean(leftbit);
		z80.N = 0;
		z80.HY = false;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU RR(CPU z80, byte val)
	{
		byte rightbit = (byte)(z80.Registers[val] << 7);
		z80.Registers[val] = (byte)((byte)(z80.Registers[val] >> 1) | Convert.ToByte(z80.CY));
		z80.CY = Convert.ToBoolean(rightbit >> 7);
		z80.N = 0;
		z80.HY = false;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU DAA(CPU z80)
	{
		byte value = 0;
		if ((byte)(z80.A & 0x0f) > 9 || z80.HY)
		{
			value = 6;
			z80.HY = (byte)(z80.A & 0x0f) + 0x06 > 0x10;
		}
		if (z80.A > 0x90 || z80.CY || ((byte)(z80.A >> 4) >= 9 && (byte)(z80.A & 0x0f) > 9))
		{
			value += 0x60;
			z80.CY = true;
		}
		ushort result = (ushort)(z80.A + value);
		z80.Z = result;
		z80.S = result;
		z80.P = (byte)result;
		z80.A = (byte)result;
		z80.ICYC = 4;
		return z80;
	}

	private static CPU JMP(CPU z80, ushort adr)
	{
		z80.PC = (ushort)(adr - 1); // Executor always increments PC by 1 so undo that
		z80.ICYC = 10;
		return z80;
	}
    private static CPU RET(CPU z80) // pop from stack
    {
        z80.SP += 2;
        z80.PC = (ushort)((z80.Mem[z80.SP - 1] << 8 | z80.Mem[z80.SP - 2]) - 1);
        z80.ICYC = 11; // most of the time
        return z80;
    }

    private static CPU CALL(CPU z80, ushort adr, byte def = 3) // push to stack
	{
		z80.SP -= 2;
		z80.MemWrite(z80.SP, (ushort)(z80.PC+def));
		JMP(z80, adr);
		z80.ICYC = 17;
		return z80;
	}

	private static CPU IndexOpcodes(CPU z80, byte irptr)
	{
		byte nbyte = z80.Mem[z80.PC + 2], nbyte2 = z80.Mem[z80.PC + 3];
		ushort nword = (ushort)(nbyte2 << 8 | nbyte);
		switch (z80.Mem[z80.PC+1])
		{
			#region Addition instrucions
			case 0x23: z80.IndexRegisters[irptr]++; z80.ICYC = 10; break;
			case 0x34: z80.MemWrite((ushort)(z80.IndexRegisters[irptr]+(sbyte)(nbyte)), (byte)(z80.Mem[z80.IndexRegisters[irptr] + (sbyte)(nbyte)]+1));
				INC(z80, z80.Mem[z80.IndexRegisters[irptr] + (sbyte)(nbyte)]);
				opbytes = 2; z80.ICYC = 10; break;
			case 0x09: ADDI(z80, z80.BC, irptr); break;
			case 0x19: ADDI(z80, z80.DE, irptr); break;
			case 0x29: ADDI(z80, z80.IndexRegisters[irptr], irptr); break;
			case 0x39: ADDI(z80, z80.SP, irptr); break;
			case 0x86: ADD(z80, z80.Mem[z80.IndexRegisters[irptr]+(sbyte)nbyte]); opbytes = 2; z80.ICYC = 19; break;
			case 0x8e: ADD(z80, z80.Mem[z80.IndexRegisters[irptr] + (sbyte)nbyte], z80.CY); opbytes = 2; z80.ICYC = 19; break;
			#endregion
			#region Subtraction instrucions
			case 0x35:
				z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)(nbyte)), (byte)(z80.Mem[z80.IndexRegisters[irptr] + (sbyte)(nbyte)]-1));
				DEC(z80, z80.Mem[z80.IndexRegisters[irptr] + (sbyte)(nbyte)]);
				opbytes = 2; z80.ICYC = 23; break;
			case 0x2b: z80.IndexRegisters[irptr]--; z80.ICYC = 10; break;
			case 0x96: SUB(z80, z80.Mem[z80.IndexRegisters[irptr] + (sbyte)nbyte]); opbytes = 2; z80.ICYC = 19; break;
			case 0x9e: SUB(z80, z80.Mem[z80.IndexRegisters[irptr] + (sbyte)nbyte], z80.CY); opbytes = 2; z80.ICYC = 19; break;
			case 0xbe: SUB(z80, z80.Mem[z80.IndexRegisters[irptr] + (sbyte)nbyte], z80.CY, false); opbytes = 2; z80.ICYC = 19; break;
			#endregion
			#region Load instructions
			case 0x21: z80.IndexRegisters[irptr] = nword; opbytes = 3; z80.ICYC = 14; break;
			case 0x22: z80.MemWrite(nword, z80.IndexRegisters[irptr]); opbytes = 3; z80.ICYC = 20; break;
			case 0x2a: z80.IndexRegisters[irptr] = z80.Mem[nword]; opbytes = 3; z80.ICYC = 20; break;
			case 0xf9: z80.SP = z80.IndexRegisters[irptr]; z80.ICYC = 10; break;
			case 0x36: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), nbyte2); opbytes = 3; z80.ICYC = 19; break;
			case 0x46: z80.B = (byte)(z80.IndexRegisters[irptr] + (sbyte)nbyte); opbytes = 2; z80.ICYC = 19; break;
			case 0x4e: z80.C = (byte)(z80.IndexRegisters[irptr] + (sbyte)nbyte); opbytes = 2; z80.ICYC = 19; break;
			case 0x56: z80.D = (byte)(z80.IndexRegisters[irptr] + (sbyte)nbyte); opbytes = 2; z80.ICYC = 19; break;
			case 0x5e: z80.E = (byte)(z80.IndexRegisters[irptr] + (sbyte)nbyte); opbytes = 2; z80.ICYC = 19; break;
			case 0x66: z80.H = (byte)(z80.IndexRegisters[irptr] + (sbyte)nbyte); opbytes = 2; z80.ICYC = 19; break;
			case 0x6e: z80.L = (byte)(z80.IndexRegisters[irptr] + (sbyte)nbyte); opbytes = 2; z80.ICYC = 19; break;
			case 0x70: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.B); opbytes = 2; z80.ICYC = 19; break;
			case 0x71: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.C); opbytes = 2; z80.ICYC = 19; break;
			case 0x72: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.D); opbytes = 2; z80.ICYC = 19; break;
			case 0x73: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.E); opbytes = 2; z80.ICYC = 19; break;
			case 0x74: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.H); opbytes = 2; z80.ICYC = 19; break;
			case 0x75: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.L); opbytes = 2; z80.ICYC = 19; break;
			case 0x77: z80.MemWrite((ushort)(z80.IndexRegisters[irptr] + (sbyte)nbyte), z80.A); opbytes = 2; z80.ICYC = 19; break;
			#endregion
			#region Special instructions
			case 0xe1: z80.IndexRegisters[irptr] = (ushort)(z80.Mem[z80.SP + 1] << 8 | z80.Mem[z80.SP]); z80.SP += 2; z80.ICYC = 14; break;
			case 0xe3: nword = z80.SP; z80.MemWrite(z80.SP, z80.IndexRegisters[irptr]); z80.IndexRegisters[irptr] = nword; z80.ICYC = 23; break; // using next word as a temp variable and shouldn't break anything
			case 0xe5: z80.SP -= 2; z80.MemWrite(z80.SP, z80.IndexRegisters[irptr]); z80.ICYC = 15; break;
            #endregion
            default: Console.WriteLine($"Opcode '0x{z80.Mem[z80.PC]:x4}' is not implemented."); break;
		}
		z80.PC += (ushort)(opbytes);
		return z80;
	}

	private static CPU MainOpcodes(CPU z80)
	{
		byte nbyte = z80.Mem[z80.PC + 1];
		ushort nword = (ushort)((z80.Mem[z80.PC + 2] << 8) | nbyte);
		switch (z80.Mem[z80.PC]) // c# optimises to a jump table anyway
		{
			#region Special instructions
			case 0x00: z80.ICYC = 4; break; // nop
			case 0x27: DAA(z80); break;
			case 0x2f: z80.A = (byte)~z80.A; z80.ICYC = 4; break;
			case 0x37: z80.N = 0; z80.HY = false; z80.CY = true; z80.ICYC = 4; break;
			case 0x3f: z80.N = 0; z80.HY = Convert.ToBoolean(z80.CY); z80.CY = !z80.CY; z80.ICYC = 4; break;
			case 0x76: z80.halt = true; z80.ICYC = 4; break; // halt
			case 0xd3: z80.PortOUT(z80, nbyte); opbytes = 2; z80.ICYC = 11; break;
			case 0xdb: z80.PortIN(z80, nbyte); opbytes = 2; z80.ICYC = 11; break;
			case 0xf3: z80.interrupt = false; z80.ICYC = 4; break;
			case 0xfb: z80.interrupt = true; z80.ICYC = 4; break;

			case 0xdd: IndexOpcodes(z80, 0); break; // IX op prefix
			case 0xfd: IndexOpcodes(z80, 1); break; // IY op prefix
			#endregion
			#region Exchange instructions
			case 0x08: ExAF(z80); break; // swap AF with AF`
			case 0xd9: (z80.Registers, z80.AltRegisters) = (z80.AltRegisters, z80.Registers); ExAF(z80); break; // swap all register pairs with alternate ones (excluding AF)
			case 0xe3: (z80.L, z80.Mem[z80.SP]) = (z80.Mem[z80.SP], z80.L); (z80.H, z80.Mem[z80.SP + 1]) = (z80.Mem[z80.SP + 1], z80.H); z80.ICYC = 19; break; // who cares about safe memory writes
			case 0xeb: (z80.DE, z80.HL) = (z80.HL, z80.DE); z80.ICYC = 4; break;
			#endregion
			#region Rotate Accumulator instructions
			case 0x07: RLC(z80, 0); break;
			case 0x17: RL(z80, 0); break;
			case 0x0f: RRC(z80, 0); break;
			case 0x1f: RR(z80, 0); break;
			#endregion
			#region INC instruction
			case 0x03: z80.BC++; z80.ICYC = 6; break;
			case 0x13: z80.DE++; z80.ICYC = 6; break;
			case 0x23: z80.HL++; z80.ICYC = 6; break;
			case 0x33: z80.SP++; z80.ICYC = 6; break;

			case 0x04: z80.B++; INC(z80, z80.B); z80.ICYC = 4; break;
			case 0x14: z80.D++; INC(z80, z80.D); z80.ICYC = 4; break;
			case 0x24: z80.H++; INC(z80, z80.H); z80.ICYC = 4; break;
			case 0x34: z80.Mem[z80.HL]++; INC(z80, z80.Mem[z80.HL]); z80.ICYC = 11; break;

			case 0x0c: z80.C++; INC(z80, z80.C); z80.ICYC = 4; break;
			case 0x1c: z80.E++; INC(z80, z80.E); z80.ICYC = 4; break;
			case 0x2c: z80.L++; INC(z80, z80.L); z80.ICYC = 4; break;
			case 0x3c: z80.A++; INC(z80, z80.A); z80.ICYC = 4; break;
			#endregion
			#region DEC instruction
			case 0x0b: z80.BC--; z80.ICYC = 6; break;
			case 0x1b: z80.DE--; z80.ICYC = 6; break;
			case 0x2b: z80.HL--; z80.ICYC = 6; break;
			case 0x3b: z80.SP--; z80.ICYC = 6; break;

			case 0x05: z80.B--; DEC(z80, z80.B); z80.ICYC = 4; break;
			case 0x15: z80.D--; DEC(z80, z80.D); z80.ICYC = 4; break;
			case 0x25: z80.H--; DEC(z80, z80.H); z80.ICYC = 4; break;
			case 0x35: z80.Mem[z80.HL]--; DEC(z80, z80.Mem[z80.HL]); z80.ICYC = 11; break;

			case 0x0d: z80.C--; DEC(z80, z80.C); z80.ICYC = 4; break;
			case 0x1d: z80.E--; DEC(z80, z80.E); z80.ICYC = 4; break;
			case 0x2d: z80.L--; DEC(z80, z80.L); z80.ICYC = 4; break;
			case 0x3d: z80.A--; DEC(z80, z80.A); z80.ICYC = 4; break;
			#endregion
			#region ADD/ADC instructions
			case 0x09: ADDHL(z80, z80.BC); break;
			case 0x19: ADDHL(z80, z80.DE); break;
			case 0x29: ADDHL(z80, z80.HL); break;
			case 0x39: ADDHL(z80, z80.SP); break;

			case 0x80: ADD(z80, z80.B); break;
			case 0x81: ADD(z80, z80.C); break;
			case 0x82: ADD(z80, z80.D); break;
			case 0x83: ADD(z80, z80.E); break;
			case 0x84: ADD(z80, z80.H); break;
			case 0x85: ADD(z80, z80.L); break;
			case 0x86: ADD(z80, z80.Mem[z80.HL]); z80.ICYC = 7; break;
			case 0x87: ADD(z80, z80.A); break;
			case 0xc6: ADD(z80, nbyte); opbytes = 2; z80.ICYC = 7; break;

			case 0x88: ADD(z80, z80.B, z80.CY); break;
			case 0x89: ADD(z80, z80.C, z80.CY); break;
			case 0x8a: ADD(z80, z80.D, z80.CY); break;
			case 0x8b: ADD(z80, z80.E, z80.CY); break;
			case 0x8c: ADD(z80, z80.H, z80.CY); break;
			case 0x8d: ADD(z80, z80.L, z80.CY); break;
			case 0x8e: ADD(z80, z80.Mem[z80.HL], z80.CY); z80.ICYC = 7; break;
			case 0x8f: ADD(z80, z80.A, z80.CY); break;
			case 0xce: ADD(z80, nbyte, z80.CY); opbytes = 2; z80.ICYC = 7; break;
			#endregion
			#region SUB/SBC instructions
			case 0x90: SUB(z80, z80.B); break;
			case 0x91: SUB(z80, z80.C); break;
			case 0x92: SUB(z80, z80.D); break;
			case 0x93: SUB(z80, z80.E); break;
			case 0x94: SUB(z80, z80.H); break;
			case 0x95: SUB(z80, z80.L); break;
			case 0x96: SUB(z80, z80.Mem[z80.HL]); z80.ICYC = 7; break;
			case 0x97: SUB(z80, z80.A); break;
			case 0xd6: SUB(z80, nbyte); opbytes = 2; z80.ICYC = 7; break;

			case 0x98: SUB(z80, z80.B, z80.CY); break;
			case 0x99: SUB(z80, z80.C, z80.CY); break;
			case 0x9a: SUB(z80, z80.D, z80.CY); break;
			case 0x9b: SUB(z80, z80.E, z80.CY); break;
			case 0x9c: SUB(z80, z80.H, z80.CY); break;
			case 0x9d: SUB(z80, z80.L, z80.CY); break;
			case 0x9e: SUB(z80, z80.Mem[z80.HL], z80.CY); z80.ICYC = 7; break;
			case 0x9f: SUB(z80, z80.A, z80.CY); break;
			case 0xde: SUB(z80, nbyte, z80.CY); opbytes = 2; z80.ICYC = 7; break;
			#endregion
			#region Logic instructions
			case 0xa0: Logic(z80, (byte)(z80.A & z80.B)); z80.HY = true; break;
			case 0xa1: Logic(z80, (byte)(z80.A & z80.C)); z80.HY = true; break;
			case 0xa2: Logic(z80, (byte)(z80.A & z80.D)); z80.HY = true; break;
			case 0xa3: Logic(z80, (byte)(z80.A & z80.E)); z80.HY = true; break;
			case 0xa4: Logic(z80, (byte)(z80.A & z80.H)); z80.HY = true; break;
			case 0xa5: Logic(z80, (byte)(z80.A & z80.L)); z80.HY = true; break;
			case 0xa6: Logic(z80, (byte)(z80.A & z80.Mem[z80.HL])); z80.HY = true; z80.ICYC = 7; break;
			case 0xa7: Logic(z80, (byte)(z80.A & z80.A)); z80.HY = true; break;
			case 0xe6: Logic(z80, (byte)(z80.A & nbyte)); z80.HY = true; opbytes = 2; z80.ICYC = 7; break;

			case 0xa8: Logic(z80, (byte)(z80.A ^ z80.B)); break;
			case 0xa9: Logic(z80, (byte)(z80.A ^ z80.C)); break;
			case 0xaa: Logic(z80, (byte)(z80.A ^ z80.D)); break;
			case 0xab: Logic(z80, (byte)(z80.A ^ z80.E)); break;
			case 0xac: Logic(z80, (byte)(z80.A ^ z80.H)); break;
			case 0xad: Logic(z80, (byte)(z80.A ^ z80.L)); break;
			case 0xae: Logic(z80, (byte)(z80.A ^ z80.Mem[z80.HL])); z80.ICYC = 7; break;
			case 0xaf: Logic(z80, (byte)(z80.A ^ z80.A)); break;
			case 0xee: Logic(z80, (byte)(z80.A ^ nbyte)); opbytes = 2; z80.ICYC = 7; break;

			case 0xb0: Logic(z80, (byte)(z80.A | z80.B)); break;
			case 0xb1: Logic(z80, (byte)(z80.A | z80.C)); break;
			case 0xb2: Logic(z80, (byte)(z80.A | z80.D)); break;
			case 0xb3: Logic(z80, (byte)(z80.A | z80.E)); break;
			case 0xb4: Logic(z80, (byte)(z80.A | z80.H)); break;
			case 0xb5: Logic(z80, (byte)(z80.A | z80.L)); break;
			case 0xb6: Logic(z80, (byte)(z80.A | z80.Mem[z80.HL])); z80.ICYC = 7; break;
			case 0xb7: Logic(z80, (byte)(z80.A | z80.A)); break;
			case 0xf6: Logic(z80, (byte)(z80.A | nbyte)); opbytes = 2; z80.ICYC = 7; break;
			#endregion
			#region CP instructions
			case 0xb8: SUB(z80, z80.B, false, false); break;
			case 0xb9: SUB(z80, z80.C, false, false); break;
			case 0xba: SUB(z80, z80.D, false, false); break;
			case 0xbb: SUB(z80, z80.E, false, false); break;
			case 0xbc: SUB(z80, z80.H, false, false); break;
			case 0xbd: SUB(z80, z80.L, false, false); break;
			case 0xbe: SUB(z80, z80.Mem[z80.HL], false, false); z80.ICYC = 7; break;
			case 0xbf: SUB(z80, z80.A, false, false); break;
			case 0xfe: SUB(z80, nbyte, false, false); opbytes = 2; z80.ICYC = 7; break;
			#endregion
			#region Pop from stack instructions
			case 0xc9: RET(z80); break;
			case 0xc0: if (z80.Z == 0) RET(z80); else z80.ICYC = 5; break;
			case 0xc8: if (z80.Z == 1) RET(z80); else z80.ICYC = 5; break;
			case 0xd0: if (!z80.CY) RET(z80); else z80.ICYC = 5; break;
			case 0xd8: if (z80.CY) RET(z80); else z80.ICYC = 5; break;
			case 0xe0: if (z80.P == 0) RET(z80); else z80.ICYC = 5; break;
			case 0xe8: if (z80.P == 1) RET(z80); else z80.ICYC = 5; break;
			case 0xf0: if (z80.S == 0) RET(z80); else z80.ICYC = 5; break;
			case 0xf8: if (z80.S == 1) RET(z80); else z80.ICYC = 5; break;

			case 0xc1: z80.C = z80.Mem[z80.SP]; z80.B = z80.Mem[z80.SP + 1]; z80.SP += 2; z80.ICYC = 10; break;
			case 0xd1: z80.E = z80.Mem[z80.SP]; z80.D = z80.Mem[z80.SP + 1]; z80.SP += 2; z80.ICYC = 10; break;
			case 0xe1: z80.L = z80.Mem[z80.SP]; z80.H = z80.Mem[z80.SP + 1]; z80.SP += 2; z80.ICYC = 10; break;
			case 0xf1: z80.F = z80.Mem[z80.SP]; z80.A = z80.Mem[z80.SP + 1]; z80.SP += 2; z80.ICYC = 10; break;
			#endregion
			#region Push to stack instructions
			case 0xcd: CALL(z80, nword); break;
			case 0xc4: if (z80.Z == 0) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xcc: if (z80.Z == 1) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xd4: if (!z80.CY) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xdc: if (z80.CY) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xe4: if (z80.P == 0) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xec: if (z80.P == 1) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xf4: if (z80.S == 0) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;
			case 0xfc: if (z80.S == 1) CALL(z80, nword); else { z80.ICYC = 10; opbytes = 3; } break;

			case 0xc5: z80.SP -= 2; z80.MemWrite(z80.SP, z80.C, z80.B); z80.ICYC = 11; break;
			case 0xd5: z80.SP -= 2; z80.MemWrite(z80.SP, z80.E, z80.D); z80.ICYC = 11; break;
			case 0xe5: z80.SP -= 2; z80.MemWrite(z80.SP, z80.L, z80.H); z80.ICYC = 11; break;
			case 0xf5: z80.SP -= 2; z80.MemWrite(z80.SP, z80.F, z80.A); z80.ICYC = 11; break;

			case 0xc7: CALL(z80, 0x00, 1); z80.ICYC = 11; break;
			case 0xcf: CALL(z80, 0x08, 1); z80.ICYC = 11; break;
			case 0xd7: CALL(z80, 0x10, 1); z80.ICYC = 11; break;
			case 0xdf: CALL(z80, 0x18, 1); z80.ICYC = 11; break;
			case 0xe7: CALL(z80, 0x20, 1); z80.ICYC = 11; break;
			case 0xef: CALL(z80, 0x28, 1); z80.ICYC = 11; break;
			case 0xf7: CALL(z80, 0x30, 1); z80.ICYC = 11; break;
			case 0xff: CALL(z80, 0x38, 1); z80.ICYC = 11; break;
			#endregion
			#region Jump instructions
			case 0xc3: JMP(z80, nword); break;
			case 0xc2: if (z80.Z == 0) JMP(z80, nword); else opbytes = 3; break;
			case 0xca: if (z80.Z == 1) JMP(z80, nword); else opbytes = 3; break; // 01000010 right, 01010010 wrong
			case 0xd2: if (!z80.CY) JMP(z80, nword); else opbytes = 3; break;
			case 0xda: if (z80.CY) JMP(z80, nword); else opbytes = 3; break;
			case 0xe2: if (z80.P == 0) JMP(z80, nword); else opbytes = 3; break;
			case 0xea: if (z80.P == 1) JMP(z80, nword); else opbytes = 3; break;
			case 0xf2: if (z80.S == 0) JMP(z80, nword); else opbytes = 3; break;
			case 0xfa: if (z80.S == 1) JMP(z80, nword); else opbytes = 3; break;
			case 0xe9: JMP(z80, z80.HL); break;

			// Relative jumps
			case 0x10: z80.ICYC = 13; z80.B--; if (z80.B != 0) z80.PC = (ushort)((sbyte)nbyte + z80.PC - 1); else { z80.ICYC = 8; opbytes = 2; } break;
			case 0x18: z80.ICYC = 12; z80.PC = (ushort)((sbyte)nbyte + z80.PC - 1); break;
			case 0x20: z80.ICYC = 12; if (z80.Z == 0) z80.PC = (ushort)((sbyte)nbyte + z80.PC - 1); else { z80.ICYC = 7; opbytes = 2; } break;
			case 0x28: z80.ICYC = 12; if (z80.Z == 1) z80.PC = (ushort)((sbyte)nbyte + z80.PC - 1); else { z80.ICYC = 7; opbytes = 2; } break;
			case 0x30: z80.ICYC = 12; if (!z80.CY) z80.PC = (ushort)((sbyte)nbyte + z80.PC - 1); else { z80.ICYC = 7; opbytes = 2; } break;
			case 0x38: z80.ICYC = 12; if (z80.CY) z80.PC = (ushort)((sbyte)nbyte + z80.PC - 1); else { z80.ICYC = 7; opbytes = 2; } break;
			#endregion
			#region LD instructions
			case 0x01: z80.BC = nword; z80.ICYC = 10; opbytes = 3; break;
			case 0x11: z80.DE = nword; z80.ICYC = 10; opbytes = 3; break;
			case 0x21: z80.HL = nword; z80.ICYC = 10; opbytes = 3; break;
			case 0x31: z80.SP = nword; z80.ICYC = 10; opbytes = 3; break;
			case 0xf9: z80.SP = z80.HL; z80.ICYC = 6; break;
			case 0x02: z80.MemWrite(z80.BC, z80.A); z80.ICYC = 7; break;
			case 0x12: z80.MemWrite(z80.DE, z80.A); z80.ICYC = 7; break;
			case 0x22: z80.MemWrite(nword, z80.HL); z80.ICYC = 16; opbytes = 3; break;
			case 0x32: z80.MemWrite(nword, z80.A); z80.ICYC = 13; opbytes = 3; break;

			case 0x0a: z80.A = z80.Mem[z80.BC]; z80.ICYC = 7; break;
			case 0x1a: z80.A = z80.Mem[z80.DE]; z80.ICYC = 7; break;
			case 0x2a: z80.H = z80.Mem[nword + 1]; z80.L = z80.Mem[nword]; z80.ICYC = 16; opbytes = 3; break;
			case 0x3a: z80.A = z80.Mem[nword]; z80.ICYC = 13; opbytes = 3; break;

			case 0x0e: z80.C = nbyte; z80.ICYC = 7; opbytes = 2; break;
			case 0x1e: z80.E = nbyte; z80.ICYC = 7; opbytes = 2; break;
			case 0x2e: z80.L = nbyte; z80.ICYC = 7; opbytes = 2; break;
			case 0x3e: z80.A = nbyte; z80.ICYC = 7; opbytes = 2; break;

			case 0x40: z80.B = z80.B; z80.ICYC = 4; break;
			case 0x41: z80.B = z80.C; z80.ICYC = 4; break;
			case 0x42: z80.B = z80.D; z80.ICYC = 4; break;
			case 0x43: z80.B = z80.E; z80.ICYC = 4; break;
			case 0x44: z80.B = z80.H; z80.ICYC = 4; break;
			case 0x45: z80.B = z80.L; z80.ICYC = 4; break;
			case 0x47: z80.B = z80.A; z80.ICYC = 4; break;

			case 0x48: z80.C = z80.B; z80.ICYC = 4; break;
			case 0x49: z80.C = z80.C; z80.ICYC = 4; break;
			case 0x4a: z80.C = z80.D; z80.ICYC = 4; break;
			case 0x4b: z80.C = z80.E; z80.ICYC = 4; break;
			case 0x4c: z80.C = z80.H; z80.ICYC = 4; break;
			case 0x4d: z80.C = z80.L; z80.ICYC = 4; break;
			case 0x4e: z80.C = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			case 0x4f: z80.C = z80.A; z80.ICYC = 4; break;

			case 0x50: z80.D = z80.B; z80.ICYC = 4; break;
			case 0x51: z80.D = z80.C; z80.ICYC = 4; break;
			case 0x52: z80.D = z80.D; z80.ICYC = 4; break;
			case 0x53: z80.D = z80.E; z80.ICYC = 4; break;
			case 0x54: z80.D = z80.H; z80.ICYC = 4; break;
			case 0x55: z80.D = z80.L; z80.ICYC = 4; break;
			case 0x57: z80.D = z80.A; z80.ICYC = 4; break;

			case 0x58: z80.E = z80.B; z80.ICYC = 4; break;
			case 0x59: z80.E = z80.C; z80.ICYC = 4; break;
			case 0x5a: z80.E = z80.D; z80.ICYC = 4; break;
			case 0x5b: z80.E = z80.E; z80.ICYC = 4; break;
			case 0x5c: z80.E = z80.H; z80.ICYC = 4; break;
			case 0x5d: z80.E = z80.L; z80.ICYC = 4; break;
			case 0x5e: z80.E = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			case 0x5f: z80.E = z80.A; z80.ICYC = 4; break;

			case 0x60: z80.H = z80.B; z80.ICYC = 4; break;
			case 0x61: z80.H = z80.C; z80.ICYC = 4; break;
			case 0x62: z80.H = z80.D; z80.ICYC = 4; break;
			case 0x63: z80.H = z80.E; z80.ICYC = 4; break;
			case 0x64: z80.H = z80.H; z80.ICYC = 4; break;
			case 0x65: z80.H = z80.L; z80.ICYC = 4; break;
			case 0x67: z80.H = z80.A; z80.ICYC = 4; break;

			case 0x68: z80.L = z80.B; z80.ICYC = 4; break;
			case 0x69: z80.L = z80.C; z80.ICYC = 4; break;
			case 0x6a: z80.L = z80.D; z80.ICYC = 4; break;
			case 0x6b: z80.L = z80.E; z80.ICYC = 4; break;
			case 0x6c: z80.L = z80.H; z80.ICYC = 4; break;
			case 0x6d: z80.L = z80.L; z80.ICYC = 4; break;
			case 0x6e: z80.L = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			case 0x6f: z80.L = z80.A; z80.ICYC = 4; break;

			case 0x70: z80.MemWrite(z80.HL, z80.B); z80.ICYC = 7; break;
			case 0x71: z80.MemWrite(z80.HL, z80.C); z80.ICYC = 7; break;
			case 0x72: z80.MemWrite(z80.HL, z80.D); z80.ICYC = 7; break;
			case 0x73: z80.MemWrite(z80.HL, z80.E); z80.ICYC = 7; break;
			case 0x74: z80.MemWrite(z80.HL, z80.H); z80.ICYC = 7; break;
			case 0x75: z80.MemWrite(z80.HL, z80.L); z80.ICYC = 7; break;
			case 0x77: z80.MemWrite(z80.HL, z80.A); z80.ICYC = 7; break;

			case 0x78: z80.A = z80.B; z80.ICYC = 4; break;
			case 0x79: z80.A = z80.C; z80.ICYC = 4; break;
			case 0x7a: z80.A = z80.D; z80.ICYC = 4; break;
			case 0x7b: z80.A = z80.E; z80.ICYC = 4; break;
			case 0x7c: z80.A = z80.H; z80.ICYC = 4; break;
			case 0x7d: z80.A = z80.L; z80.ICYC = 4; break;
			case 0x7e: z80.A = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			case 0x7f: z80.A = z80.A; z80.ICYC = 4; break;

			case 0x06: z80.B = nbyte; z80.ICYC = 7; opbytes = 2; break;
			case 0x16: z80.D = nbyte; z80.ICYC = 7; opbytes = 2; break;
			case 0x26: z80.H = nbyte; z80.ICYC = 7; opbytes = 2; break;
			case 0x36: z80.MemWrite(z80.HL, nbyte); z80.ICYC = 10; opbytes = 2; break;
			case 0x46: z80.B = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			case 0x56: z80.D = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			case 0x66: z80.H = z80.Mem[z80.HL]; z80.ICYC = 7; break;
			#endregion
			default: Console.WriteLine($"Opcode '0x{z80.Mem[z80.PC]:x4}' is not implemented."); break;
		}
		return z80;
	}
	public static CPU Step(CPU z80)
	{
#if DEBUG
		Console.WriteLine($"PC: {z80.PC:X4}, AF: {z80.AF:X4}, BC: {z80.BC:X4}, DE: {z80.DE:X4}, HL: {z80.HL:X4}, SP: {z80.SP:X4}, Opcode {z80.Mem[z80.PC]:X2}");
#endif
		MainOpcodes(z80);
		z80.PC += (ushort)opbytes;
		opbytes = 1;
		return z80;
	}

	static void Main(string[] args)
	{
		CPU z80 = new(65535);
		byte[] rom = File.ReadAllBytes("prelim.com");
		Array.Copy(rom, 0, z80.Mem, 0x100, rom.Length);
		Console.WriteLine($"Loaded ROM (prelim)");
		File.WriteAllBytes("dump.bin", z80.Mem);
		z80.PC = 0x100;
		z80.AF = 0xffff;
		z80.SP = 0xffff;

		// inject "out 0,a" at 0x0000 (signal to stop the test)
		z80.Mem[0x0000] = 0xD3;
		z80.Mem[0x0001] = 0x00;
		z80.Mem[0x0005] = 0xD3;
		z80.Mem[0x0006] = 0x01;
		z80.Mem[0x0007] = 0xC9;

		while (!z80.halt)
		{
			Step(z80);
		}
        File.WriteAllBytes("dump.bin", z80.Mem);
    }
}
