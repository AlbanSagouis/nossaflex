# parsing works as expected

    Code
      parsing_nossaflex(reading_nossaflex(path = testthat::test_path("testdata",
        "nossaflex_filenames.txt")))
    Output
                T     NO     SS      A     FL     EX
           <char> <char> <char> <char> <char> <char>
      1: est roll     01    250    2.8     50     +2
      2: est roll     02    250    2.8     50     -1

