#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 2a sample input" {
    run diff <(./day2.js --initial < t/day2.test.in) t/day2.test.out.a
    [ "$status" -eq 0 ]
}

@test "day 2a actual input" {
    run diff <(./day2.js --initial < t/day2.in) t/day2.out.a
    [ "$status" -eq 0 ]
}

@test "day 2b sample input" {
    run diff <(./day2.js < t/day2.test.in) t/day2.test.out.b
    [ "$status" -eq 0 ]
}

@test "day 2b actual input" {
    run diff <(./day2.js --initial < t/day2.in) t/day2.out.a
    [ "$status" -eq 0 ]
}
