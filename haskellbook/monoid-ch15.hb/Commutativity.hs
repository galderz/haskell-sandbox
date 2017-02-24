module Commutativity where


numPlusComm =
    76 + 67 == 67 + 76


numProdComm =
    76 * 67 == 67 * 76


evilPlus =
    flip (+)


evilPlusIsComm =
    76 + 67 == 76 `evilPlus` 67


evilPlusPlus =
    flip (++)


oneList =
    [1..3]


otherList =
    [4..6]


evilPlusPlusNotComm =
    oneList ++ otherList /=
        oneList `evilPlusPlus` otherList
