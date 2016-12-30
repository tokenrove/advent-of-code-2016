#include <stdio.h>

enum { N = 3017957 };
/* enum { N = 5 }; */
static unsigned presents[N];

static unsigned next(unsigned elf)
{
    unsigned next_elf = (1+elf) % N;
    while (!presents[next_elf] && next_elf != elf)
        next_elf = (1+next_elf) % N;
    return next_elf;
}

int main(void)
{
    for (int i = 0; i < N; ++i) presents[i] = 1;

    unsigned elf = 0, next_elf = 0;
    do {
        next_elf = next(elf);
        if (next_elf == elf) break;
        presents[elf] += presents[next_elf];
        presents[next_elf] = 0;
        printf("%u steals %u's presents (%u)\n", 1+elf, 1+next_elf, presents[elf]);
        elf = next(elf);
    } while(next_elf != elf);
    printf("%u\n", 1+elf);
    return 0;
}
