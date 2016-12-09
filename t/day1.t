#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 1a sample input 1" {
    output=$(echo 'R2, L3' | ./day1 --initial)
    [ "$output" = "5" ]
}

@test "day 1a sample input 2" {
    output=$(echo 'R2, R2, R2' | ./day1 --initial)
    [ "$output" = "2" ]
}

@test "day 1a sample input 3" {
    output=$(echo 'R5, L5, R5, R3' | ./day1 --initial)
    [ "$output" = "12" ]
}

@test "day 1a" {
    run diff <(./day1 --initial < t/day1.in) t/day1.out.a
    [ "$status" -eq 0 ]
}

@test "day 1b sample input 1" {
    output=$(echo 'R8, R4, R4, R8' | ./day1)
    [ "$output" = "4" ]
}

@test "day 1b" {
    run diff <(./day1 < t/day1.in) t/day1.out.b
    [ "$status" -eq 0 ]
}
