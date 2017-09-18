{----------------------------------------------------------------------------------------------------------------------
    NAME:       Sara Oonk
    ASSIGNMENT: Lab 3, exercise 2 - Testing 'parse'
    TIME TAKEN: 3h preparation:
                        - Studying lecture 3 (mainly to fully understand 'parse' and related functions)
                        - Studying lecture 2 for test methods and better grasping of test properties
                        - Considering approaches, proposed them to Hugo & Ana
                        - Finalizing approach, structuring and documenting

    DESCRIPTION:
    The lecture notes of this week define a function parse for parsing propositional formulas.
    Test this function. You can use any test method you want.
    Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.

    APPROACH:
    ---- AUTOMATED VS MANUAL ----
    Automated test methods are most valuable when they draw their domain (test data) from a generator, because this way
    the domain is the largest and most random, not decided by human hands, and not limited to ideas that may not be complete.

    ---- TESTING USING GENERATORS ----
    The Hoare Test with relevance is an interesting way to perform automated tests whilst making sure that the randomly
    generated data set is actually relevant (enough) in order to be able to call the test reliable. However, for testing
    'parse' I would require a generator that generates formulas, which is a little more complex than just random int lists.

    ---- REQUIREMENTS OF MY GENERATOR ----
    If I want to do Hoare testing with relevance, it would not make sense if all of my generated formulas match the
    precondition of the test, because then the relevance would always be 1.0. So if I want to test 'parse' in an automated
    way while simultaneously exploring the concept of relevance, my formula generator must return imperfect formulas as
    well as proper formulas.

{----------------------------------------------------------------------------------------------------------------------}


module Lab3_2 where
 import Lecture3



