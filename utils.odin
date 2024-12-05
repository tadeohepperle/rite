package rite

import "core:container/queue"
import "core:fmt"
import "core:hash"
import "core:log"
import "core:slice"

print :: fmt.println
info :: log.info
todo :: proc(loc := #caller_location) -> ! {
	panic("todo at ", loc)
}
tprint :: proc(args: ..any) -> string {
	return fmt.aprint(..args, allocator = context.temp_allocator, sep = "")
}
Err :: Maybe(string)
Empty :: struct {}

BUCKET_SIZE :: 32
BucketArray :: struct($T: typeid) {
	buckets:         [dynamic]^[BUCKET_SIZE]T,
	last_bucket:     ^[BUCKET_SIZE]T,
	last_bucket_len: int, // index into last bucket
}
bucket_array_insert :: proc(arr: ^BucketArray($T), el: T) -> ^T {
	if arr.last_bucket == nil {
		arr.last_bucket = new([BUCKET_SIZE]T)
		append(&arr.buckets, arr.last_bucket)
	}
	el_ptr := &arr.last_bucket[arr.last_bucket_len]
	el_ptr^ = el
	arr.last_bucket_len += 1
	if arr.last_bucket_len == BUCKET_SIZE {
		arr.last_bucket = nil
		arr.last_bucket_len = 0
	}
	return el_ptr
}
bucket_array_free :: proc(arr: ^BucketArray($T)) {
	for b in arr.buckets {
		free(b)
	}
}
bucket_array_len :: proc(arr: BucketArray($T)) -> int {
	sum := 0
	sum += max(len(arr.buckets) - 1, 0) * BUCKET_SIZE
	sum += arr.last_bucket_len
	return sum
}
bucket_array_print :: proc(arr: BucketArray($T)) {
	print("Bucket Array with", bucket_array_len(arr), "elements:")
	for i in 0 ..< len(arr.buckets) - 1 {
		for el, j in arr.buckets[i] {
			print("    ", i, j, el)
		}
	}
	if arr.last_bucket != nil {
		for el, j in arr.last_bucket[:arr.last_bucket_len] {
			print("    ", len(arr.buckets) - 1, j, el)

		}
	}
}


HASHER_SEED :: u64(0xcbf29ce484222325)
Hasher :: struct {
	hash: u64,
}

hasher_init :: #force_inline proc() -> Hasher {
	return Hasher{hash = HASHER_SEED}
}
hasher_add :: proc {
	hasher_add_bytes,
	hasher_add_string,
	hasher_add_type_hash,
}
hasher_add_bytes :: proc "contextless" (hasher: ^Hasher, bytes: []u8) {
	hasher.hash = hash.fnv64a(bytes, seed = hasher.hash)
}
hasher_add_string :: proc "contextless" (hasher: ^Hasher, s: string) {
	hasher_add_bytes(hasher, transmute([]u8)s)
}
// hasher_add_sized :: proc "contextless" (hasher: ^Hasher, val: $T) where T != string {
// 	val := val
// 	bytes := slice.bytes_from_ptr(&val, size_of(T))
// 	hasher_add_bytes(hasher, bytes)
// }
hasher_add_type_hash :: proc "contextless" (hasher: ^Hasher, val: TypeHash) {
	val := val
	bytes := slice.bytes_from_ptr(&val, size_of(u64))
	hasher_add_bytes(hasher, bytes)
}
hasher_finish :: proc "contextless" (hasher: Hasher) -> u64 {
	return hasher.hash
}


tmp_arr :: proc($T: typeid, cap: int = 0) -> [dynamic]T {
	return make([dynamic]T, 0, cap, allocator = context.temp_allocator)
}
tmp_slice :: proc($T: typeid, len: int = 0) -> []T {
	return make([]T, len, allocator = context.temp_allocator)
}
tmp_map :: proc($K: typeid, $V: typeid) -> map[K]V {
	return make(map[K]V, allocator = context.temp_allocator)
}
