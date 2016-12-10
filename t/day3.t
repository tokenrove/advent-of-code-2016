#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 3a" {
    run diff <(./day3.apl -- --initial t/day3.in) t/day3.out.a
    [ "$status" = "0" ]
}

@test "day 3b" {
    run diff <(./day3.apl -- t/day3.in) t/day3.out.b
    [ "$status" = "0" ]
}
