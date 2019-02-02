#!/usr/bin/env bash

TEST_FILE="test-program.koak"

test() {
    echo;
    name="$1";
    program="$2";
    expected="$3";
    echo -n "$program" > "$TEST_FILE";
    output=$(./koak "$TEST_FILE");
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

test 'IfExpr' \
     'if i == 2 then 3 + 2 else -1;' \
     'Parsed ([ExprStmt (If (IfExpr {if_cond = Bin (BinExpr {bin_op = "==", bin_lhs = Ident "i", bin_rhs = Lit (IntLiteral 2)}), if_then = Bin (BinExpr {bin_op = "+", bin_lhs = Lit (IntLiteral 3), bin_rhs = Lit (IntLiteral 2)}), if_else = Just (Un (UnExpr {un_op = "-", un_arg = Lit (IntLiteral 1)}))}))],"")';

test 'ForExpr' \
     'for i = 2, i < 4, i = i + 1 in fib(i, 3 + 2);' \
     'Parsed ([ExprStmt (For (ForExpr {for_init = Bin (BinExpr {bin_op = "=", bin_lhs = Ident "i", bin_rhs = Lit (IntLiteral 2)}), for_cond = Bin (BinExpr {bin_op = "<", bin_lhs = Ident "i", bin_rhs = Lit (IntLiteral 4)}), for_oper = Bin (BinExpr {bin_op = "=", bin_lhs = Ident "i", bin_rhs = Bin (BinExpr {bin_op = "+", bin_lhs = Ident "i", bin_rhs = Lit (IntLiteral 1)})}), for_body = Call (CallExpr {call_ident = "fib", call_args = [Ident "i",Bin (BinExpr {bin_op = "+", bin_lhs = Lit (IntLiteral 3), bin_rhs = Lit (IntLiteral 2)})]})}))],"")';

test 'Precedence 1' \
     '3 + 2 * 3;' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "+", bin_lhs = Lit (IntLiteral 3), bin_rhs = Bin (BinExpr {bin_op = "*", bin_lhs = Lit (IntLiteral 2), bin_rhs = Lit (IntLiteral 3)})}))],"")';

test 'Precedence 2' \
     '3 * 2 + 3;' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "+", bin_lhs = Bin (BinExpr {bin_op = "*", bin_lhs = Lit (IntLiteral 3), bin_rhs = Lit (IntLiteral 2)}), bin_rhs = Lit (IntLiteral 3)}))],"")';

test 'Precedence 3' \
     '3 * -2 + 3;' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "+", bin_lhs = Bin (BinExpr {bin_op = "*", bin_lhs = Lit (IntLiteral 3), bin_rhs = Un (UnExpr {un_op = "-", un_arg = Lit (IntLiteral 2)})}), bin_rhs = Lit (IntLiteral 3)}))],"")';

test 'Precedence 4' \
     '1 + 2 * 3;' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "+", bin_lhs = Lit (IntLiteral 1), bin_rhs = Bin (BinExpr {bin_op = "*", bin_lhs = Lit (IntLiteral 2), bin_rhs = Lit (IntLiteral 3)})}))],"")';

test 'Precedence 5' \
     '1 * 2 + 3;' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "+", bin_lhs = Bin (BinExpr {bin_op = "*", bin_lhs = Lit (IntLiteral 1), bin_rhs = Lit (IntLiteral 2)}), bin_rhs = Lit (IntLiteral 3)}))],"")';

test 'Precedence 6' \
     '(1 + 2) * 3;' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "*", bin_lhs = Bin (BinExpr {bin_op = "+", bin_lhs = Lit (IntLiteral 1), bin_rhs = Lit (IntLiteral 2)}), bin_rhs = Lit (IntLiteral 3)}))],"")';

test 'Precedence 7' \
     '1 * (2 + 3);' \
     'Parsed ([ExprStmt (Bin (BinExpr {bin_op = "*", bin_lhs = Lit (IntLiteral 1), bin_rhs = Bin (BinExpr {bin_op = "+", bin_lhs = Lit (IntLiteral 2), bin_rhs = Lit (IntLiteral 3)})}))],"")';

echo;
