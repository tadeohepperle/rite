package rite

import "core:fmt"

print :: fmt.println
todo :: proc(loc := #caller_location) -> ! {
	panic("todo at ", loc)
}
tprint :: proc(args: ..any) -> string {
	return fmt.aprint(..args, allocator = context.temp_allocator, sep = "")
}
Err :: Maybe(string)
