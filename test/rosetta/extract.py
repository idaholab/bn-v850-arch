#!/usr/bin/env python3
"""Extract (address, bytes, asm) tuples from a ccrh-built ELF object + .asm pair.

Pairs the raw .text bytes of each function symbol (from the ELF) with the
ccrh-emitted assembler source lines for that function. The output is a JSON
list of {name, addr, bytes_hex, asm} records — our ground truth for lift/
disasm tests.
"""
import json, re, struct, sys, pathlib

def read_u32(b, o): return struct.unpack_from('<I', b, o)[0]
def read_u16(b, o): return struct.unpack_from('<H', b, o)[0]

def parse_elf(path: pathlib.Path):
    d = path.read_bytes()
    assert d[:4] == b'\x7fELF'
    # 32-bit LE
    e_shoff = read_u32(d, 0x20)
    e_shentsize = read_u16(d, 0x2e)
    e_shnum = read_u16(d, 0x30)
    e_shstrndx = read_u16(d, 0x32)
    sections = []
    for i in range(e_shnum):
        o = e_shoff + i * e_shentsize
        sh_name = read_u32(d, o)
        sh_type = read_u32(d, o + 4)
        sh_offset = read_u32(d, o + 16)
        sh_size = read_u32(d, o + 20)
        sh_link = read_u32(d, o + 24)
        sh_info = read_u32(d, o + 28)
        sh_entsize = read_u32(d, o + 36)
        sections.append(dict(name_off=sh_name, type=sh_type, offset=sh_offset,
                             size=sh_size, link=sh_link, info=sh_info,
                             entsize=sh_entsize))
    shstr_off = sections[e_shstrndx]['offset']
    def strz(base, off):
        e = d.index(b'\x00', base + off)
        return d[base + off:e].decode('latin-1')
    for s in sections:
        s['name'] = strz(shstr_off, s['name_off'])
    symtab = next((s for s in sections if s['name'] == '.symtab'), None)
    strtab_off = sections[symtab['link']]['offset'] if symtab else None
    funcs = []
    if symtab:
        n = symtab['size'] // symtab['entsize']
        for i in range(n):
            o = symtab['offset'] + i * symtab['entsize']
            st_name = read_u32(d, o)
            st_value = read_u32(d, o + 4)
            st_size = read_u32(d, o + 8)
            st_info = d[o + 12]
            st_shndx = read_u16(d, o + 14)
            if (st_info & 0xf) != 2:  # STT_FUNC
                continue
            if st_shndx >= len(sections):
                continue
            sec = sections[st_shndx]
            name = strz(strtab_off, st_name)
            raw = d[sec['offset'] + st_value : sec['offset'] + st_value + st_size]
            funcs.append(dict(name=name, addr=st_value, size=st_size,
                              bytes_hex=raw.hex(), section=sec['name']))
    return funcs

ASM_LABEL = re.compile(r'^([A-Za-z_][A-Za-z_0-9]*):\s*$')
ASM_INSN  = re.compile(r'^\s+([a-z][a-z0-9.]*)\b(.*)$')

def parse_asm(path: pathlib.Path):
    """Return {func_name: [asm_line, ...]}."""
    out = {}
    cur = None
    for line in path.read_text().splitlines():
        m = ASM_LABEL.match(line)
        if m:
            cur = m.group(1)
            out[cur] = []
            continue
        if cur and ASM_INSN.match(line):
            out[cur].append(line.strip())
    return out

def main(obj_path):
    obj = pathlib.Path(obj_path)
    asm = obj.with_suffix('.asm')
    funcs = parse_elf(obj)
    asms = parse_asm(asm)
    recs = []
    for f in funcs:
        f['asm'] = asms.get(f['name'], [])
        recs.append(f)
    print(json.dumps(recs, indent=2))

if __name__ == '__main__':
    main(sys.argv[1])
