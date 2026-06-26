#include <stdio.h>
#include <stdint.h>

uint8_t interpret(uint8_t* instructions, uint32_t* index) {
    uint8_t x, y;
    uint32_t i = *index;
    *index += 1;
    switch (instructions[i]) {
        case 0x00: /* constant */
            x = instructions[*index];
            *index += 1;
            return x;
        case 0x01: /* addition */
            x = interpret(instructions, index);
            y = interpret(instructions, index);
            return x + y;
        case 0x02: /* subtraction */
            x = interpret(instructions, index);
            y = interpret(instructions, index);
            return x - y;
        case 0x03: /* multiplication */
            x = interpret(instructions, index);
            y = interpret(instructions, index);
            return x * y;
    }
    return 0;
}

int main() {
    /* 2 + 3 */
    uint8_t instructions[] = { 1, 0, 2, 0, 3 };
    uint32_t index = 0;
    uint8_t result = interpret(instructions, &index);

    printf("Result: %d\n", result);
}
