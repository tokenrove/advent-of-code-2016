#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 20a" {
    run diff <(./day20 --initial < t/day20.in) t/day20.out.a
    [ "$status" -eq 0 ]
}

@test "day 20b" {
    run diff <(./day20 < t/day20.in) t/day20.out.b
    [ "$status" -eq 0 ]
}
