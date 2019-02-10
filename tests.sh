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
    echo "$output" | grep -F "$expected" > /dev/null;
    if [ $? -eq "0" ]; then
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
     '[Ann () (ExprStmt (Ann () (If (Ann () (IfExpr {if_cond = Ann () (Bin (Ann () (BinExpr {bin_op = "==", bin_lhs = Ann () (Ident (Ann () "i")), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))}))), if_then = Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Lit (Ann () (IntLiteral 3))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))}))), if_else = Just (Ann () (Un (Ann () (UnExpr {un_op = "-", un_rhs = Ann () (Lit (Ann () (IntLiteral 1)))}))))})))))]';

test 'ForExpr' \
     'for i = 2, i < 4, i = i + 1 in fib(i, 3 + 2);' \
     '[Ann () (ExprStmt (Ann () (For (Ann () (ForExpr {for_init = Ann () (Bin (Ann () (BinExpr {bin_op = "=", bin_lhs = Ann () (Ident (Ann () "i")), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))}))), for_cond = Ann () (Bin (Ann () (BinExpr {bin_op = "<", bin_lhs = Ann () (Ident (Ann () "i")), bin_rhs = Ann () (Lit (Ann () (IntLiteral 4)))}))), for_oper = Ann () (Bin (Ann () (BinExpr {bin_op = "=", bin_lhs = Ann () (Ident (Ann () "i")), bin_rhs = Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Ident (Ann () "i")), bin_rhs = Ann () (Lit (Ann () (IntLiteral 1)))})))}))), for_body = Ann () (Call (Ann () (CallExpr {call_ident = "fib", call_args = [Ann () (Ident (Ann () "i")),Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Lit (Ann () (IntLiteral 3))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))})))]})))})))))]';

test 'Precedence 1' \
     '3 + 2 * 3;' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Lit (Ann () (IntLiteral 3))), bin_rhs = Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Lit (Ann () (IntLiteral 2))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))})))))]';

test 'Precedence 2' \
     '3 * 2 + 3;' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Lit (Ann () (IntLiteral 3))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))}))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))))]';

test 'Precedence 3' \
     '3 * -2 + 3;' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Lit (Ann () (IntLiteral 3))), bin_rhs = Ann () (Un (Ann () (UnExpr {un_op = "-", un_rhs = Ann () (Lit (Ann () (IntLiteral 2)))})))}))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))))]';

test 'Precedence 4' \
     '1 + 2 * 3;' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Lit (Ann () (IntLiteral 1))), bin_rhs = Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Lit (Ann () (IntLiteral 2))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))})))))]';

test 'Precedence 5' \
     '1 * 2 + 3;' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Lit (Ann () (IntLiteral 1))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))}))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))))]';

test 'Precedence 6' \
     '(1 + 2) * 3;' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Lit (Ann () (IntLiteral 1))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 2)))}))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))))]';

test 'Precedence 7' \
     '1 * (2 + 3);' \
     '[Ann () (ExprStmt (Ann () (Bin (Ann () (BinExpr {bin_op = "*", bin_lhs = Ann () (Lit (Ann () (IntLiteral 1))), bin_rhs = Ann () (Bin (Ann () (BinExpr {bin_op = "+", bin_lhs = Ann () (Lit (Ann () (IntLiteral 2))), bin_rhs = Ann () (Lit (Ann () (IntLiteral 3)))})))})))))]';

echo;
