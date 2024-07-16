# Out of Order CPU
## Design Specifications
Here is an introduction to OoOCpu

- The CPU supports the complete rv32im instruction set, which can execute basic arithmetic, logic, shift, branch, jump, integer multiplication, integer division and other instructions.

- The CPU supports privileged instructions, which can implement system calls and exception handling, and only supports machine mode here.

- The CPU supports the processing of interrupts and exceptions, can respond to external interrupts and internal exceptions, and when an interrupt exception occurs, the hardware is responsible for recording the cause of the exception, the pc at the time of the exception, etc, and automatically jumps to the TRAP entry program pointed to by mtvec. Software is responsible for context protection, and hardware returns to continue executing the original program after receiving the mret instruction.

- The multiplier is structured using Radix-4 Booth Algorithm and Wallace tree. Radix-4 Booth Algorithm reduces partial products generation to 1/2. The wallace tree uses a carry save-adders (CSA), which compresses the partial product terms by a ratio of 3:2 to speed up the addition process.

- The number of cycles required by the multi-cycle divider is determined by the divisor and dividend. In RISC-V, dividing by zero or overflowing does not produce an exception, and the division operation is finished in a single cycle. In other cases, the first nonzero value of the dividend is judged, and then the first quotient is calculated.

- The CPU includes 32 general-purpose registers for storing operands and results, with the x0 register fixed at 0.

- The CPU includes basic CSR registers for storing control and status information, such as interrupt enable (mie), interrupt suspend (mip), exception code (mcause), etc.

- The CPU is designed with a seven-stage pipeline, including fetch, decode, register renaming, issue, execute, writeback, and retire.

- The CPU adopts the Harvard architecture, and the instruction and data storage are separated and accessed by itcm and spm, respectively. Single-cycle reading improves memory access efficiency.

- The data width of itcm and spm is 32 bits, and one word of data can be read or written at a time. The capacity of itcm and spm can be configured as needed.

- At the memory access module can access the bus and decide whether to access the spm or the bus by address decoding, where the bus satisfies the AHB-lite protocol.

- CLINT is used to generate software interrupts and timer interrupts. There is an msip register, which is triggered by software, and there is a 64-bit mtime timer, which is counted by a low-frequency clock and triggers the timer interrupt when its value is equal to the value in the mtimecmp register. All three registers can be read and written by the bus.

- Supports UART transmission, 115200 baud rate, transmission based on one start bit, eight data bits, one odd parity bit and one end bit. Since the design is a 32-bit processor, each 32-bit data is transmitted in four times. At the same time, there is a 32-bit FIFO with a depth of 8 in the uart module, which can store 8 uart-tx data.

- Support digital display, CPU can read and write to digital register, support 0-f output.

![1](https://github.com/meosii/OoOcpu/blob/master/ooocpu.drawio.pdf)
