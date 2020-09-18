#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   [1] "car"  "goat" "goat"
#'   [1] "goat" "car"  "goat"
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Select a door for the contestant
#' @description
#' 
#' This function randomly selects a door for the contestant. 
#' The prize behind the door will not be revealed.
#' 
#' @details
#' This function randomly selects a door for the contestant. 
#' At this point in the game all three doors are closed and the contestant
#' will have a 33% chance of picking the door with a car. The doors vector 
#' consist of numbers 1-3, using the function sample() to pick one door and 
#' store the value in a.pick.
#' 
#' @param 
#' no arugements are used by the function.
#' @return 
#' The funtion returns a single number 1-3. as a.pick
#' @examples
#' 
#' [1] 1
#' [1] 3
#' 
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a Goat door
#' @description
#' This function opens one of the two remaining doors to reveal a Goat.
#' @details
#' 
#' This function uses if() statements to open one of the remaining doors 
#' that has a goat behind it. The condiditions of the if() statements will
#' only open a door that the contestant hasn't yet picked and it must have 
#' a goat behind it.
#' 
#' @param 
#' arguements assigned to this function are the game, which shows the location
#' of the goats and car for this specific game and a.pick which is the door 
#' randomly picked for the contestant.
#' @return 
#' This function will return a door 1-3 that was not already selected and ust have a goat.
#' @examples
#' open_goat_door()
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Door
#' @description
#' This function changes the contestants door to the remaining closed door.
#' @details
#' The function takes the values stored in opened.door and a.pick to provide 
#' simulate if the contestant switches doors or keeps their original door. The 
#' value is then stored in final.pick. 
#' @param 
#' open.door is an integer value, and a.pick is also an integer value
#' @return 
#' final.pick is an integer value
#' @examples
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Winner
#' @description
#' This function compares the values stored in final.pick and game 
#' to determine if the contestant picked a car door for both if they 
#' stayed and if they switched doors.
#' @details
#' The functions uses the values in final.pick, which stores the original 
#' door and the switch door, and the values in game, which stores what is 
#' behind each door in this particular game. The first if statement returns
#'  "Win" if the door has a car behind it. 
#' @param 
#' 
#' @return 
#' The function returns "WIN" if there is a car behind the door and "LOSE" 
#' if there is a goat.
#' @examples
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play Game
#' @description
#' The function will play the game from start to finish one time.
#' @details
#' The function will play one single round of the montyhall game and 
#' output the results. The results are broken down by startegy and will
#' let you know wehter you won or lost as a result to your stay or switch strategy.
#' @param 
#' @return 
#' The function returns a data frames with the results of the game.
#' @examples
#'   strategy outcome
#'     stay    LOSE
#'   switch     WIN
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}

play_game()




#' @title
#' Play the Game N times
#' @description
#' This function simulates the results for the game played N times.
#' @details
#' This function playes the game N times and stores the results into
#'  a dataframe. It utilizes a for loop from 1:n and in this case n is 100.
#'  The for loop runs through the game n=100 times and stores the results
#'   in the results.list dataframe.
#' @param 
#' @return 
#' The function returns a data frame stored in results.df. 
#' @examples
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
