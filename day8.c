/* C implementation of day 8.
 * Lots of dirty tricks so it feels like C.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>


int main(void)
{
    enum { W = 50, H = 6 };
    /* enum { W = 7, H = 3 }; */
    uint64_t m[H] = {0}, o[H];
    _Static_assert(8*sizeof(*m) >= W, "m not wide enough");
    uint64_t bit(unsigned i) { return 1UL<<i; }
    uint64_t mask(unsigned i) { return bit(i)-1; }

    int count(void) {
        int sum = 0;
        for (int i = 0; i < H; ++i)
            sum += __builtin_popcountl(m[i] & mask(W));
        return sum;
    }

    enum {COMMAND_MAX = 128};
    char command[COMMAND_MAX];
    while (1 == scanf(" %[a-z]", command)) {
        switch (htonl(*(uint32_t *)command)) {
        case 0x72656374:
        {
            unsigned w, h;
            scanf("%ux%u", &w, &h);
            assert(w <= W && h <= H);
            while (h--) m[h] |= mask(w);
            break;
        }
        case 0x726f7461:
        {
            char axis;
            unsigned position, magnitude;
            scanf(" %*[a-z] %c=%u by %u", &axis, &position, &magnitude);
            if ('x' == axis) {
                assert(position < W);
                memcpy(o, m, sizeof(o));
                for (unsigned i = 0; i < H; ++i) {
                    /* FUCK C modulo semantics */
                    m[i] = (o[(H + i - magnitude)%H] & bit(position)) |
                        (o[i] & ~bit(position));
                }
            } else if ('y' == axis) {
                assert(position < H);
                unsigned r = magnitude % W;
                m[position] = ((m[position] << r)&mask(W)) |
                    ((m[position] >> (W-r))&mask(r));
            } else
                abort();
            break;
        }
        default: abort();
        }
    }
    for (unsigned i = 0; i < H; ++i) {
        for (unsigned j = 0; j < W; ++j) {
            if (0 == j%5)
                putchar(' ');
            putchar(m[i]&bit(j) ? '#':'.');
        }
        putchar('\n');
    }
    printf("%d\n", count());
    return 0;
}
