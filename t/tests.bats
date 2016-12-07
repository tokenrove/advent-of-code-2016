#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 1a sample input 1" {
    run echo 'R2, L3' | ./day1
    [ "$status" -eq 0 ]
    [ "$output" = "5" ]
}

@test "day 1a sample input 2" {
    run echo 'R2, R2, R2' | ./day1
    [ "$status" -eq 0 ]
    [ "$output" = "2" ]
}

@test "day 1a sample input 3" {
    run echo 'R5, L5, R5, R3' | ./day1
    [ "$status" -eq 0 ]
    [ "$output" = "12" ]
}

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
