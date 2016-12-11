#!/usr/bin/env bats
# -*- shell-script -*-

@test "day 9a sample input 1" {
    output=$(echo 'ADVENT' | ./day9 --initial)
    [ "$output" = "6" ]
}

@test "day 9a sample input 2" {
    output=$(echo 'A(1x5)BC' | ./day9 --initial)
    [ "$output" = "7" ]
}

@test "day 9a sample input 3" {
    output=$(echo '(3x3)XYZ' | ./day9 --initial)
    [ "$output" = "9" ]
}

@test "day 9a sample input 4" {
    output=$(echo 'A(2x2)BCD(2x2)EFG' | ./day9 --initial)
    [ "$output" = "11" ]
}

@test "day 9a sample input 5" {
    output=$(echo '(6x1)(1x3)A' | ./day9 --initial)
    [ "$output" = "6" ]
}

@test "day 9a sample input 6" {
    output=$(echo 'X(8x2)(3x3)ABCY' | ./day9 --initial)
    [ "$output" = "18" ]
}

@test "day 9b sample input 1" {
    output=$(echo '(3x3)XYZ' | ./day9)
    [ "$output" = "9" ]
}

@test "day 9b sample input 2" {
    output=$(echo 'X(8x2)(3x3)ABCY' | ./day9)
    [ "$output" = "20" ]
}

@test "day 9b sample input 3" {
    output=$(echo '(27x12)(20x12)(13x14)(7x10)(1x12)A' | ./day9)
    [ "$output" = "241920" ]
}

@test "day 9b sample input 4" {
    output=$(echo '(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN' | ./day9)
    [ "$output" = "445" ]
}

@test "day 9a" {
    run diff <(./day9 --initial < t/day9.in) t/day9.out.a
    [ "$status" = "0" ]
}

@test "day 9b" {
    run diff <(./day9 < t/day9.in) t/day9.out.b
    [ "$status" = "0" ]
}
