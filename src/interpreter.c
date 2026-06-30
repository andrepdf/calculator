#include <stdint.h>
#include <string.h>

#define CON 0x00
#define NEG 0x01
#define ADD 0x02
#define SUB 0x03
#define MUL 0x04
#define DIV 0x05

double interpret(const uint8_t* bytes, uint32_t* index) {
    double result = 0;

    uint8_t instr = bytes[*index];
    *index += 1;

    switch (instr) {
        case CON:
            memcpy(&result, bytes + *index, sizeof(double));
            *index += sizeof(double);
            break;
        case NEG:
            result -= interpret(bytes, index);
            break;
        case ADD:
            result = interpret(bytes, index);
            result += interpret(bytes, index);
            break;
        case SUB:
            result = interpret(bytes, index);
            result -= interpret(bytes, index);
            break;
        case MUL:
            result = interpret(bytes, index);
            result *= interpret(bytes, index);
            break;
        case DIV:
            result = interpret(bytes, index);
            double right = interpret(bytes, index);
            if (right == 0.0) {
                result = 0.0 / 0.0;
            } else {
                result /= right;
            }
            break;
        default:
            break;
    }

    return result;
}
