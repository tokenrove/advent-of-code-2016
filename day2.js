#!/usr/bin/env nodejs

var g = []
if ('--initial' == process.argv[2]) {
    for (var i = 1; i <= 9; ++i) g[i] = {u:i,d:i,l:i,r:i,i:i}
    for (var i = 0; i < 6; ++i) {
        var h = 1 + (i&1) + 3*(i>>1), v = 1+i
        g[h].r = 1+h; g[h+1].l = h; g[v].d = v+3; g[v+3].u = v
    }
} else {
    for (var i = 1; i <= 13; ++i) g[i] = {u:i,d:i,l:i,r:i,i:i}
    var u = [3,6,7,8,10,11,12,13],
        l = [3,4,6,7,8,9,11,12]
    for (var i = 0; i < 8; ++i) {
        var d = u[i]-(i&&(7-i)?4:2); g[u[i]].u = d; g[d].d = u[i];
        var r = l[i]-1; g[l[i]].l = r; g[r].r = l[i];
    }
}
var x = 5
function main() {
    for (var c of (process.stdin.read() || [])) {
        if (10 == c)
            process.stdout.write(x.toString(16))
        else
            x = g[x][String.fromCharCode(c|0x20)]
    }
}
process.stdin.on('readable', main)
process.stdin.on('end', function(){main();process.stdout.write("\n")})
