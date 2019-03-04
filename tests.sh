#!/usr/bin/env bash

TEST_FILE="test-program.koak"

test() {
    echo;
    name="$1";
    program="$2";
    expected="$3";
    echo -n "$program" > "$TEST_FILE";
    output=$(./koak "$TEST_FILE" 2> /dev/null);
    rm "$TEST_FILE";
    if [ "$output" = "$expected" ]; then
        echo "$name: OK !";
    else
        echo "$name: KO !";
        echo "Program: $program"
        echo "Got: $output";
        echo "Expected: $expected";
        exit 1;
    fi
}

test 'If condition' \
     'i = 2; if i == 2 then 3 + 2 else -1; i = 1; if i == 2 then 3 + 2 else -1;' \
     $'2\n5\n1\n-1';

test 'For loop' \
     'ret = 0; for i = 2, i < 4, i = i + 1 in ret = ret + i;' \
     $'0\n5';

test 'While loop' \
     'i = 0; while i < 10 do i = i + 1;' \
     $'0\n10';

test 'Function definition' \
     'def add2(x:integer): integer x + 2; add2(40);' \
     $'42';

test 'Binary operator definition' \
     'def binary && (a:integer b:integer): integer if a then b else a; 42 && 53; 0 && 4;' \
     $'53\n0';

test 'Unary operator definition' \
     'def unary % (a:integer): integer if a then 0 else 1; %53; %0;' \
     $'0\n1';

test 'Precedence 1' \
     '3 + 2 * 3;' \
     $'9';

test 'Precedence 2' \
     '3 * 2 + 3;' \
     $'9';

test 'Precedence 3' \
     '3 * -2 + 3;' \
     $'-3';

test 'Precedence 4' \
     '1 + 2 * 3;' \
     $'7';

test 'Precedence 5' \
     '1 * 2 + 3;' \
     $'5';

test 'Precedence 6' \
     '(1 + 2) * 3;' \
     $'9';

test 'Precedence 7' \
     '1 * (2 + 3);' \
     $'5';

test 'Associativity' \
     'a = b = 1;' \
     $'1';

test 'Lambdas (Simple)' \
     'f = (x -> x * 2); f(8);' \
     $'16';

test 'Lambdas (Advanced inference)' \
     'f = (x -> x * 2.0); f(8);' \
     $'16.000000';

echo;
