import Test.HUnit

testAddZero :: Test
testAddZero = "addZero" ~: do
    assertEqual "FIll string with spaces" "* *  *  " (addZero "* *  *")

