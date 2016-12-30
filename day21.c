#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char s[] = "abcdefgh";
/* static char s[] = "abcde"; */
static unsigned n = sizeof(s)-1;

static void reverse(unsigned x, unsigned y)
{
    assert(x < n); assert(y < n);
    if (x > y) { unsigned t = x; x = y; y = t; }
    for (; x < y; ++x, --y) {
        char t = s[x]; s[x] = s[y]; s[y] = t;
    }
}

static void rotate(int m)
{
    m = (m + n) % n;
    if (m == 0) return;
    reverse(0, m-1);
    reverse(m, n-1);
    reverse(0, n-1);
}


struct insn {
    enum { SWAP, SWAP_CHAR, ROTATE, ROTATE_CHAR, REVERSE, MOVE } op;
    unsigned x, y;
};


static void exec(struct insn i)
{
    switch (i.op) {
    case SWAP: {
        char t = s[i.x]; s[i.x] = s[i.y]; s[i.y] = t;
        break;
    }

    case SWAP_CHAR: {
        char *p = strchr(s, i.x),
             *q = strchr(s, i.y);
        char t = *p; *p = *q; *q = t;
        break;
    }

    case ROTATE:
        rotate(i.x);
        break;

    case ROTATE_CHAR: {
        int p = index(s, i.x)-s;
        p += (p >= 4) ? 2 : 1;
        rotate(n-p);
        break;
    }

    case REVERSE:
        reverse(i.x, i.y);
        break;

    case MOVE:
        reverse(i.x, i.y);
        reverse(i.x, i.y + ((i.x > i.y) ? 1 : -1));
        break;

    default:
        abort();
    }
}


static bool attempt(struct insn *insns, size_t n_insns)
{
    for (size_t pc = 0; pc < n_insns; ++pc)
        exec(insns[pc]);
    return 0 == strcmp(s, "fbgdceah");
}

int main(void)
{
    char *line = NULL;
    size_t len = 0;
    ssize_t n_read;
    struct insn insns[128] = {0};
    size_t n_insns = 0;

    while ((n_read = getline(&line, &len, stdin)) != -1) {
        unsigned x, y;
        char a, b;
        if (2 == sscanf(line, "swap position %u with position %u",
                        &x, &y)) {
            insns[n_insns++] = (struct insn){.op = SWAP, .x = x, .y = y};
        } else if (2 == sscanf(line, "swap letter %c with letter %c",
                               &a, &b)) {
            insns[n_insns++] = (struct insn){.op = SWAP_CHAR, .x = a, .y = b};
        } else if (1 == sscanf(line, "rotate left %u steps", &x)) {
            insns[n_insns++] = (struct insn){.op = ROTATE, .x = x};
        } else if (1 == sscanf(line, "rotate right %u steps", &x)) {
            insns[n_insns++] = (struct insn){.op = ROTATE, .x = n-x};
        } else if (1 == sscanf(line, "rotate based on position of letter %c", &a)) {
            insns[n_insns++] = (struct insn){.op = ROTATE_CHAR, .x = a};
        } else if (2 == sscanf(line, "reverse positions %u through %u",
                               &x, &y)) {
            insns[n_insns++] = (struct insn){.op = REVERSE, .x = x, .y = y};
        } else if (2 == sscanf(line, "move position %u to position %u",
                               &x, &y)) {
            insns[n_insns++] = (struct insn){.op = MOVE, .x = x, .y = y};
        } else abort();
    }

    char *u = strdup(s);
    unsigned c[sizeof(s)/sizeof(s[0])] = {0};
    size_t i = 0;
    while (i < n) {
        if (c[i] < i) {
            unsigned t;
            if (0 == (i%2)) {
                t = u[0]; u[0] = u[i]; u[i] = t;
            } else {
                t = u[c[i]]; u[c[i]] = u[i]; u[i] = t;
            }

            strcpy(s, u);
            if (attempt(insns, n_insns)) {
                printf("%s\n", u);
                return 0;
            }
            ++c[i];
            i = 0;
        } else {
            c[i] = 0;
            ++i;
        }
    }
    printf("not found\n");
}
