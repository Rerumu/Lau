## Lua Assembly Utility

`lau` is a bare bones tool for tinkering with an assembly representation of Lua. It supports an IR as a front-end for programmatically and manually altering Lua code. The disassembler can turn Lua bytecode files into IR, and the assembler can turn IR back into Lua bytecode files.

The program is currently implemented as a command line tool, and usage can be observed via `lau -h`.

Only Lua 5.4 is supported as of now. You may need to configure the type declarations to fit the Lua platform you are targeting.
