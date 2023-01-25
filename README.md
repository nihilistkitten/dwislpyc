# 394 HW5

By Riley Shahar, based on earlier work with Sima Nerush.

Features:

- function calls (with arbitrary number of arguments)
- conditionals
- booleans and integers
- while loops

For example, when given this code:
```
def foo() {
    x: int = input(0);
    print(x);
    return x + 4;
}

x: int = 1;
if 2 < x {
    print(x);
} else {
    while foo() < 10 {
        print(2);
    }
}
```

It generates this:
```
LBL l1
ENTER
SET r4 0
PTI r4
GTI r3
MOV r5 r3
PTI r5
MOV r7 r3
SET r8 4
ADD r6 r7 r8
RTN r6
JMP l2
LBL l2
LEAVE
SET r9 1
SET r14 2
MOV r15 r9
BLT r14 r15 l16 l17
LBL l16
SET r10 1
JMP l18
LBL l17
SET r10 0
LBL l18
BEQZ r10 l12 l11
LBL l11
MOV r19 r9
PTI r19
JMP l13
LBL l12
LBL l21
CLL l1
RTN r24
SET r25 10
BLT r24 r25 l26 l27
LBL l26
SET r20 1
JMP l28
LBL l27
SET r20 0
LBL l28
BEQZ r20 l23 l22
LBL l22
SET r29 2
PTI r29
JMP l21
LBL l23
LBL l13

```
