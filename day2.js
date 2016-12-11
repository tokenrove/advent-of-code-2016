#!/usr/bin/env nodejs

var g = [], x = 5
o = (i) => () => {process.stdout.write(i.toString(16)); return i}
f = (i) => () => i
for (var i = 1; i <= 13; ++i)
    g[i] = {u:f(i),d:f(i),l:f(i),r:f(i),i:f(i),'*':o(i)}
if ('--initial' == process.argv[2]) {
    for (var i = 0; i < 6; ++i) {
        var h = 1 + (i&1) + 3*(i>>1), v = 1+i
        g[h].r = f(1+h); g[h+1].l = f(h); g[v].d = f(v+3); g[v+3].u = f(v)
    }
} else {
    var u = [3,6,7,8,10,11,12,13],
        l = [3,4,6,7,8,9,11,12]
    for (var i = 0; i < 8; ++i) {
        var d = u[i]-(i&&(7-i)?4:2); g[u[i]].u = f(d); g[d].d = f(u[i]);
        var r = l[i]-1; g[l[i]].l = f(r); g[r].r = f(l[i]);
    }
}

function main() { for (var c of (process.stdin.read() || []))
    x = g[x][String.fromCharCode(c|0x20)]() }
process.stdin.on('readable', main)
process.stdin.on('end', function(){main();process.stdout.write("\n")})
