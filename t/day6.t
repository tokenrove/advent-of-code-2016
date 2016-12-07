#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 6a sample input" {
    run ./day6.ijs --initial < t/day6.test.in
    [ "$output" = "easter" ]
}

@test "day 6a input" {
    run diff <(./day6.ijs --initial < t/day6.in) ./t/day6.out.a
    [ "$status" -eq 0 ]
}

@test "day 6b sample input" {
    run ./day6.ijs < t/day6.test.in
    [ "$output" = "advent" ]
}

@test "day 6b input" {
    run diff <(./day6.ijs < t/day6.in) ./t/day6.out.b
    [ "$status" -eq 0 ]
}
