namespace z80cs
{
	class CPU(ushort memsize)
	{
		public byte[] Mem { get; set; } = new byte[memsize];
		public ushort IX=0, IY=0, SP=0, PC=0;
		public byte I=0, R=0;
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
		public ushort AF { get { return (ushort)((A << 8) & F); } set { A = (byte)(value >> 8); F = (byte)(value & 0xf); } }
		public ushort BC { get { return (ushort)((B << 8) & C); } set { B = (byte)(value >> 8); C = (byte)(value & 0xf); } }
		public ushort DE { get { return (ushort)((D << 8) & E); } set { D = (byte)(value >> 8); E = (byte)(value & 0xf); } }
		public ushort HL { get { return (ushort)((H << 8) & L); } set { H = (byte)(value >> 8); L = (byte)(value & 0xf); } }


		public byte data = 0; // used as a temp byte
		private static bool Parity(ushort ans) // left shift by 1 and add 0 if 0 and 1 if 1, then keep left shifting 1
		{
			byte oddcount = 0;
			for (byte i = 0; i < 8; i++)
			{
				oddcount += (byte)((ans >> i) & 1);
			}
			return (oddcount & 1) == 0;
		}
		// byte/ushort flags since "getting" them sets S, Z, P and CY
		public byte S { get { return (byte)(F & 128); } set { F |= (byte)(value & 128); } }
		public byte Z { get { return (byte)(F & 64); } set { F |= (byte)(Convert.ToByte(value == 0) << 6); } }
		public byte P { get { return (byte)(F & 4); } set { F |= (byte)(Convert.ToByte(Parity(value)) << 2); } }
		public ushort CY { get { return (ushort)(F & 1); } set { F |= Convert.ToByte(value > 0xff); } }
		public bool HY { get { return (F & 16) == 1; } set { F |= (byte)(Convert.ToByte(value) << 4); } } // not done
		public bool V { get { return (F & 4) == 1; } set { F |= (byte)(Convert.ToByte(value) << 2); } } // same bit as parity, only inc, dec, and the sub/add instructions
		public bool N { get { return (F & 2) == 1; } set { F |= (byte)(Convert.ToByte(value) << 1); } } // not done; add = false, sub = true
		public void MemWrite(ushort adr, byte val)
		{
			if (adr < 65535) // program crashes if it tries to write to somewhere that doesn't exist anyway
			{
				Console.WriteLine($"Warning: Tried to write to {adr:x4}, which is in ROM");
			}
			Mem[adr] = val;
		}
		public void MemWrite(ushort adr, byte hival, byte loval)
		{
			MemWrite(adr, hival);
			MemWrite((ushort)(adr + 1), loval);
		}
		public void MemWrite(ushort adr, ushort val)
		{
			MemWrite((ushort)(adr + 1), (byte)(val >> 8));
			MemWrite(adr, (byte)val);
		}
	}

	class InstructionHelpers
	{
		protected static CPU DEC(CPU z80, byte result)
		{
			z80.N = true;
			z80.S = result;
			z80.Z = result;
			z80.V = result == 127;
			z80.HY = ((result+1) & 0xf) == 0;
			return z80;
		}
	}

	class Run : InstructionHelpers
	{
		private static byte inst_cyc = 0;
		private static byte opbytes = 1;
		public static CPU z80 = new(65535);
		private static CPU MainAddress()
		{
			byte nbyte = z80.Mem[z80.PC + 1];
			ushort nword = (ushort)((z80.Mem[z80.PC + 2] << 8) & z80.Mem[z80.PC + 1]);
			switch (z80.Mem[z80.PC]) // c# optimises to a jump table anyway
			{
#region NOP
				case 0x00: inst_cyc = 4; break;
#endregion
#region INC instruction
				case 0x03: z80.BC++;	inst_cyc = 6; break;
				case 0x13: z80.DE++; inst_cyc = 6; break;
				case 0x23: z80.HL++; inst_cyc = 6; break;
				case 0x33: z80.SP++; inst_cyc = 6; break;

				case 0x04: z80.B++; inst_cyc = 4; break;
				case 0x14: z80.D++; inst_cyc = 4; break;
				case 0x24: z80.H++; inst_cyc = 4; break;
				case 0x34: z80.Mem[z80.HL]++; inst_cyc = 11; break;

				case 0x0c: z80.C++; inst_cyc = 4; break;
				case 0x1c: z80.E++; inst_cyc = 4; break;
				case 0x2c: z80.L++; inst_cyc = 4; break;
				case 0x3c: z80.A++; inst_cyc = 4; break;
#endregion
#region DEC instruction
				case 0x05: z80.B--; inst_cyc = 4; break;
				case 0x15: z80.D--; inst_cyc = 4; break;
				case 0x25: z80.H--; inst_cyc = 4; break;
				case 0x35: z80.Mem[z80.HL]--; inst_cyc = 11; break;

				case 0x0b: z80.BC--; inst_cyc = 6; break;
				case 0x1b: z80.DE--; inst_cyc = 6; break;
				case 0x2b: z80.HL--; inst_cyc = 6; break;
				case 0x3b: z80.SP--; inst_cyc = 6; break;

				case 0x0d: z80.C--; inst_cyc = 4; break;
				case 0x1d: z80.E--; inst_cyc = 4; break;
				case 0x2d: z80.L--; inst_cyc = 4; break;
				case 0x3d: z80.A--; inst_cyc = 4; break;
#endregion
#region LD instructions
				case 0x01: z80.BC = nword; inst_cyc = 10; opbytes = 3; break;
				case 0x11: z80.DE = nword; inst_cyc = 10; opbytes = 3; break;
				case 0x21: z80.HL = nword; inst_cyc = 10; opbytes = 3; break;
				case 0x31: z80.SP = nword; inst_cyc = 10; opbytes = 3; break;
				case 0x02: z80.MemWrite(z80.BC, z80.A); inst_cyc = 7; break;
				case 0x12: z80.MemWrite(z80.DE, z80.A); inst_cyc = 7; break;
				case 0x22: z80.MemWrite(nword, z80.HL); inst_cyc = 16; opbytes = 3; break;
				case 0x32: z80.MemWrite(nword, z80.A); inst_cyc = 13; opbytes = 3; break;

				case 0x0a: z80.A = z80.Mem[z80.BC]; inst_cyc = 7; break;
				case 0x1a: z80.A = z80.Mem[z80.DE]; inst_cyc = 7; break;
				case 0x2a: z80.H = z80.Mem[nword+1]; z80.L = z80.Mem[nword]; inst_cyc = 16; break;
				case 0x3a: z80.A = z80.Mem[nword]; inst_cyc = 13; break;

				case 0x0e: z80.C = nbyte; inst_cyc = 7; opbytes = 2; break;
				case 0x1e: z80.E = nbyte; inst_cyc = 7; opbytes = 2; break;
				case 0x2e: z80.L = nbyte; inst_cyc = 7; opbytes = 2; break;
				case 0x3e: z80.A = nbyte; inst_cyc = 7; opbytes = 2; break;

				case 0x40: z80.B = z80.B; inst_cyc = 4; break;
				case 0x41: z80.B = z80.C; inst_cyc = 4; break;
				case 0x42: z80.B = z80.D; inst_cyc = 4; break;
				case 0x43: z80.B = z80.E; inst_cyc = 4; break;
				case 0x44: z80.B = z80.H; inst_cyc = 4; break;
				case 0x45: z80.B = z80.L; inst_cyc = 4; break;
				case 0x47: z80.B = z80.A; inst_cyc = 4; break;

				case 0x48: z80.C = z80.B; inst_cyc = 4; break;
				case 0x49: z80.C = z80.C; inst_cyc = 4; break;
				case 0x4a: z80.C = z80.D; inst_cyc = 4; break;
				case 0x4b: z80.C = z80.E; inst_cyc = 4; break;
				case 0x4c: z80.C = z80.H; inst_cyc = 4; break;
				case 0x4d: z80.C = z80.L; inst_cyc = 4; break;
				case 0x4e: z80.C = z80.Mem[z80.HL]; inst_cyc = 7; break;
				case 0x4f: z80.C = z80.A; inst_cyc = 4; break;

				case 0x50: z80.D = z80.B; inst_cyc = 4; break;
				case 0x51: z80.D = z80.C; inst_cyc = 4; break;
				case 0x52: z80.D = z80.D; inst_cyc = 4; break;
				case 0x53: z80.D = z80.E; inst_cyc = 4; break;
				case 0x54: z80.D = z80.H; inst_cyc = 4; break;
				case 0x55: z80.D = z80.L; inst_cyc = 4; break;
				case 0x57: z80.D = z80.A; inst_cyc = 4; break;

				case 0x58: z80.E = z80.B; inst_cyc = 4; break;
				case 0x59: z80.E = z80.C; inst_cyc = 4; break;
				case 0x5a: z80.E = z80.D; inst_cyc = 4; break;
				case 0x5b: z80.E = z80.E; inst_cyc = 4; break;
				case 0x5c: z80.E = z80.H; inst_cyc = 4; break;
				case 0x5d: z80.E = z80.L; inst_cyc = 4; break;
				case 0x5e: z80.E = z80.Mem[z80.HL]; inst_cyc = 7; break;
				case 0x5f: z80.E = z80.A; inst_cyc = 4; break;

				case 0x60: z80.H = z80.B; inst_cyc = 4; break;
				case 0x61: z80.H = z80.C; inst_cyc = 4; break;
				case 0x62: z80.H = z80.D; inst_cyc = 4; break;
				case 0x63: z80.H = z80.E; inst_cyc = 4; break;
				case 0x64: z80.H = z80.H; inst_cyc = 4; break;
				case 0x65: z80.H = z80.L; inst_cyc = 4; break;
				case 0x67: z80.H = z80.A; inst_cyc = 4; break;

				case 0x68: z80.L = z80.B; inst_cyc = 4; break;
				case 0x69: z80.L = z80.C; inst_cyc = 4; break;
				case 0x6a: z80.L = z80.D; inst_cyc = 4; break;
				case 0x6b: z80.L = z80.E; inst_cyc = 4; break;
				case 0x6c: z80.L = z80.H; inst_cyc = 4; break;
				case 0x6d: z80.L = z80.L; inst_cyc = 4; break;
				case 0x6e: z80.L = z80.Mem[z80.HL]; inst_cyc = 7; break;
				case 0x6f: z80.L = z80.A; inst_cyc = 4; break;

				case 0x70: z80.MemWrite(z80.HL, z80.B); inst_cyc = 7; break;
				case 0x71: z80.MemWrite(z80.HL, z80.C); inst_cyc = 7; break;
				case 0x72: z80.MemWrite(z80.HL, z80.D); inst_cyc = 7; break;
				case 0x73: z80.MemWrite(z80.HL, z80.E); inst_cyc = 7; break;
				case 0x74: z80.MemWrite(z80.HL, z80.H); inst_cyc = 7; break;
				case 0x75: z80.MemWrite(z80.HL, z80.L); inst_cyc = 7; break;
				case 0x77: z80.MemWrite(z80.HL, z80.A); inst_cyc = 7; break;

				case 0x78: z80.A = z80.B; inst_cyc = 4; break;
				case 0x79: z80.A = z80.C; inst_cyc = 4; break;
				case 0x7a: z80.A = z80.D; inst_cyc = 4; break;
				case 0x7b: z80.A = z80.E; inst_cyc = 4; break;
				case 0x7c: z80.A = z80.H; inst_cyc = 4; break;
				case 0x7d: z80.A = z80.L; inst_cyc = 4; break;
				case 0x7e: z80.A = z80.Mem[z80.HL]; inst_cyc = 7; break;
				case 0x7f: z80.A = z80.A; inst_cyc = 4; break;

				case 0x06: z80.B = nbyte; inst_cyc = 7; opbytes = 2; break;
				case 0x16: z80.D = nbyte; inst_cyc = 7; opbytes = 2; break;
				case 0x26: z80.H = nbyte; inst_cyc = 7; opbytes = 2; break;
				case 0x36: z80.MemWrite(z80.HL, nbyte); inst_cyc = 10; opbytes = 2; break;
				case 0x46: z80.B = z80.Mem[z80.HL]; inst_cyc = 7; break;
				case 0x56: z80.D = z80.Mem[z80.HL]; inst_cyc = 7; break;
				case 0x66: z80.H = z80.Mem[z80.HL]; inst_cyc = 7; break;
#endregion

				case 0x76: break; // halt

				// Exchange instructions
				case 0x08: // swap AF with AF`
					(z80.AltRegisters[0], z80.A) = (z80.A, z80.AltRegisters[0]);
					(z80.AltRegisters[1], z80.F) = (z80.F, z80.AltRegisters[1]);
					inst_cyc = 4;
					break;
				default:
					Console.WriteLine($"Opcode '0x{z80.Mem[z80.PC]:x4}' is not implemented.");
					break;
			}
			return z80;
		}
		public static CPU Step(CPU z80)
		{
			//MainAddress();
			return z80;
		}
	}
}
