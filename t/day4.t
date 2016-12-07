#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 4a sample input" {
    run ./day4 --initial <<EOF
aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]
EOF
    [ "$output" = "1514" ]
}

@test "day 4a" {
    run diff <(./day4 --initial < t/day4.in) t/day4.out.a
    [ "$status" = "0" ]
}

@test "day 4b" {
    run diff <(./day4 < t/day4.in) t/day4.out.b
    [ "$status" = "0" ]
}
