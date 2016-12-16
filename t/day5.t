#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 5a sample input" {
    run ./day5 --initial abc
    [ "$output" = "18f47a30" ]
}

@test "day 5b sample input" {
    run ./day5 abc
    [ "$output" = "05ace8e3" ]
}

key=$(cat t/day5.in)

@test "day 5a" {
    run diff <(./day5 --initial $key) t/day5.out.a
    [ "$status" = "0" ]
}

@test "day 5b" {
    run diff <(./day5 $key) t/day5.out.b
    [ "$status" = "0" ]
}
