module Identity where


-- zero is identity value for addition
plusIdentity =
    521 + 0 == 521


-- one is identity value for multiplication
productIdentity =
    521 * 1 == 521


myList =
    [1..424242]


plusIdentityInList =
    map (+0) myList == myList


productIdentityInList =
    map (*1) myList == myList
