#!/usr/bin/env python2
import md5
import re
import sys

easy_mode = '--initial' in sys.argv
secret = sys.argv[-1] + '{}'

cache = {}

def hash(i):
    if cache.has_key(i):
        return cache[i]
    s = md5.new(secret.format(i)).hexdigest()
    cache[i] = s
    if easy_mode: return s
    for j in range(0,2016):
        s = md5.new(s).hexdigest()
    cache[i] = s
    return s

N = 40000
fives = []
for i in range(1,N):
    s = hash(i)
    if re.search(r'(.)\1\1\1\1', s):
        fives.append((i,s))

key = []
idxs = {}
for (i,s) in fives:
    c = re.search(r'(.)\1\1\1\1', s).group(1)
    for j in range(max(0,i-1000), i-1):
        s = hash(j)
        m = re.search(r'(.)\1\1', s)
        if m and m.group(1) == c and not idxs.has_key(j):
            key.append(c)
            idxs[j] = (i,c)

print sorted(idxs)[63]
