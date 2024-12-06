# Final-project
Ryan Lamoreaux
Final project for PHC 6099

The file app.r has the entirety of the code in order to run my game. The additional files are all pictures that I made in order to avoid potential licensing issues with pictures, as well as to make a much more simple and aesthetically appropriate set of dice to match the app overall. 

Within the app itself, I tried grouping the like elements together, as well as adding comments both within and outside of certain functions to help parse what is happening at all times. 

Aesthetically, depending on the size of the monitor/laptop/device the user is on, certain elements may be slightly offcenter or possibly overlapping. In this regard, I am mainy talking about the images of the dice. I resized them to be as aesthetically pleasing on both a fullscreen monitor as well as slightly windowed on a laptop.

I go into detail on how to use the app in my final report, but I do believe that the app is highly streamlined and sensible, especially if you have any history of playing Yahtzee, especially online or on an app previously. 

I found the easiest method to launch the game is to simply open rstudio and input the following code:

library(shiny)
runGitHub("Final-project", "rmlamoreaux")
