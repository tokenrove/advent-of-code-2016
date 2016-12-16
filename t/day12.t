#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 12a sample input" {
    run diff <(./day12 --initial < t/day12.test.in) t/day12.test.out
    [ "$status" -eq 0 ]
}

@test "day 12a" {
    run diff <(./day12 --initial < t/day12.in) t/day12.out.a
    [ "$status" -eq 0 ]
}

@test "day 12b" {
    run diff <(./day12 < t/day12.in) t/day12.out.b
    [ "$status" -eq 0 ]
}
