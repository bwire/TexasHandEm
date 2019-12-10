#Evolution gaming home task

I used no packages for this task and I think all data structures are self-explanatory.
The main idea was to make all calculations inside the Option monad, cause we have some input data and we need to make sure all the data is correct.
As a result we get a string output, in case of failure we get string message - so i decided not to exit the Option until the finish line.

Also I did not want to use any external libraries like Cats, so I had to write some helper functions like `traverse`.

The main algorithm is to structure initial game from the input data as a Board and Hands, then get all possible 5-card combination for every Hand, then find the proper HandValue for each combination, then find the best HandValue for each hand and finally make some transformations to get the result.
The entry point is in the TexasHandEm.scala file.

Thank you.

