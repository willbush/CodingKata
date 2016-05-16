### homework 2

http://www.seas.upenn.edu/~cis194/spring13/lectures.html

In homework2 `Log.hs` is separated from `LogAnalysis.hs` mainly because of how they want to test it when the homework is turned in. I combined what was `Log.hs` into `LogAnalysis` because I'm not turning anything in but rather doing this for fun and to learn Haskell.

I also went with different names for all of the given functions in `Log.hs` and some of the functions that needed to be created because they were too vague and/or not verbs.

I changed the name and behavior of their `whatWentWrong` and `testWhatWentWrong` functions to `filterLogFileByErrorLevelThat` and `filterLogMsgsByErrorLevelThat` respectively. In stead of a hard-coded predicate and magic number of 50 to filter with, I changed to so that a predicate `(ErrorLevel -> Bool)` function can be passed in to filter with. In addition, instead of passing in the other functions as parameters, they are called directly since they are now in the same scope. Similarly, `testParse` was renamed to `takeFromLogFile` and the parse function is also called directly.


Other name changes:

`buildTree` instead of `build`

`sortLogMessages` instead of `inOrder`
