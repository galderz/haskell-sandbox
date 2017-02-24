module Associativity where


numPlusAssoc =
    (1 + 9001) + 9001 ==
        1 + (9001 + 9001)


numProdAssoc =
    (7 * 8) * 3
        == 7 * (8 * 3)


numMinusNonAssoc =
    (1 - 10) - 100
        == 1 - (10 - 100)
