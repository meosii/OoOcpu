test_name = csr
as:
	riscv32-unknown-elf-c++ -nostdlib -nostdinc -static -g -Ttext 0x80000000 $(test_name).s -o $(test_name).elf -march=rv32im -mabi=ilp32
objdump:
	riscv32-unknown-linux-gnu-objdump $(test_name).elf -d > out
