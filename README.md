# z80cs
A portable Zilog z80 emulator, written in C#.

Some notes:
 - Some instructions are still yet to be implemented, but most of them are and it will pass a basic preliminary test suite. 
 - Emulation isn't currently 1:1 with original hardware or other fully functional emulators, which is mainly due to the flags not working as expected in this emulator.

## Running 
### 1. Install prerequisites and clone repo 
Make sure you have at least .NET 8 and git installed, then
```
git clone https://github.com/tmaakis/z80cs.git && cd z80cs
```
### 2. Run the progran  
```
dotnet run path/to/rom -c Release
```
## Resources used 
* [z80 opcode table](https://clrhome.org/table/)
* [z80 flag affection from z80.info](https://www.z80.info/z80sflag.htm)
* [z80 emu in C by superzazu](https://github.com/superzazu/z80)
* [Test binary](https://github.com/begoon/z80exer/blob/master/cpm/zexall.com)
