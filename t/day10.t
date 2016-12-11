#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 10a sample input" {
    run diff <(./day10.native --initial < t/day10.test.in.a) t/day10.test.out.a
    [ "$status" = "0" ]
}

# @test "day 10a" {
#     run diff <(./day10.native --initial < t/day10.test.in.a) t/day10.test.out.a
#     [ "$status" = "0" ]
# }
