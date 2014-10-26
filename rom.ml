
let create_rom filename =
  let rom_name = (Filename.chop_suffix filename ".net") ^ ".rom" in
  let rom = open_in rom_name in
  assert false
