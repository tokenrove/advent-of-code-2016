#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 1a sample input 1" {
    output=$(echo 'R2, L3' | ./day1)
    [ "$output" = "5" ]
}

@test "day 1a sample input 2" {
    output=$(echo 'R2, R2, R2' | ./day1)
    [ "$output" = "2" ]
}

@test "day 1a sample input 3" {
    output=$(echo 'R5, L5, R5, R3' | ./day1)
    [ "$output" = "12" ]
}
