#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 7a sample input" {
    run ./day7 --initial < t/day7.test.in.a
    [ "$output" = "2" ]
}

@test "day 7b sample input" {
    run ./day7 < t/day7.test.in.b
    [ "$output" = "3" ]
}

@test "day 7a" {
    run diff <(./day7 --initial < t/day7.in) t/day7.out.a
    [ "$status" = "0" ]
}

@test "day 7b" {
    run diff <(./day7 < t/day7.in) t/day7.out.b
    [ "$status" = "0" ]
}
