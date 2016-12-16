#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 14a sample input" {
    run ./day14.py --initial abc
    [ "$output" = "22728" ]
}

@test "day 14b sample input" {
    run ./day14.py abc
    [ "$output" = "22551" ]
}

key=$(cat t/day14.in)

@test "day 14a" {
    run diff <(./day14.py --initial $key) t/day14.out.a
    [ "$status" = "0" ]
}

@test "day 14b" {
    run diff <(./day14.py $key) t/day14.out.b
    [ "$status" = "0" ]
}
