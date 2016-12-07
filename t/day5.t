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
