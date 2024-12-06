library(shiny)

## function for image rendering
imageconvert <- function(x){
  if(x == 0)
    return("dieZero.jpg")
  else if(x == 1)
    return("dieOne.jpg")
  else if(x == 2)
    return("dieTwo.jpg")
  else if(x == 3)
    return("dieThree.jpg")
  else if(x == 4)
    return("dieFour.jpg")
  else if(x == 5)
    return("dieFive.jpg")
  else if(x == 6)
    return("dieSix.jpg")
  else{}
}

options <- c("", "Ones", "Twos", "Threes", "Fours", "Fives", "Sixes", "3 of a kind", "4 of a kind", "Full House", "Small Straight", "Large Straight", "Yahtzee", "Chance")

## return to here
ui <- fluidPage(


  ## dice pictures
  fluidRow(
    column(width = 1,
           fluidRow(div(style = "height: 150px;", imageOutput(outputId = "die1"))),
           fluidRow(style = "text-align: center;", checkboxInput(inputId = "die1check", label = "Hold"))
           
    ),
    column(width = 1,
           fluidRow(div(style = "height: 150px;", imageOutput(outputId = "die2"))),
           fluidRow(style = "text-align: center", checkboxInput(inputId = "die2check", label = "Hold"))
    ),
    column(width = 1,
           fluidRow(div(style = "height: 150px;", imageOutput(outputId = "die3"))),
           fluidRow(style = "text-align: center; height: 100px", checkboxInput(inputId = "die3check", label = "Hold")),
           fluidRow(style = "text-align: center", actionButton(inputId = "roll", label = "Roll", style = "height: 50px; width: 100px; font-size: 150%" ))
    ),
    column(width = 1,
           fluidRow(div(style = "height: 150px;", imageOutput(outputId = "die4"))),
           fluidRow(style = "text-align: center", checkboxInput(inputId = "die4check", label = "Hold"))
    ),
    column(width = 1,
           fluidRow(div(style = "height: 150px;", imageOutput(outputId = "die5"))),
           fluidRow(style = "text-align: center;", checkboxInput(inputId = "die5check", label = "Hold"))
    ),
    column(width = 4, offset = 2, height = 12,
           tableOutput(outputId = "scorecard"))
    
  ),
  
  ## others below checkboxes
  textOutput(outputId = "noMore"),
  selectInput(inputId = "selectScore", label = "Score to input", choices = options),
  textOutput(outputId = "message"),
  textOutput(outputId = "bonusYahtzee"),
  actionButton(inputId = "submit", label = "Submit"),
  textOutput(outputId = "finalturn"),
  
  
  
)


server <- function(input, output, session){
  ## reactive values initialization
  temp <- reactiveValues(score = 0)
  times <- reactiveValues(rolltimes = 0)
  yahtzee <- reactiveValues(counter = 0)
  dicevals <- reactiveValues(dice_vals = c(0, 0, 0, 0, 0))
  keep <- reactiveValues(keep_die = c(0, 0, 0, 0, 0))
  timekeep <- reactiveValues(turns = 0)
  chosen <- reactiveValues(x = c())
  imgvals <- reactiveValues(img_vals = c("dieZero.jpg", "dieZero.jpg", "dieZero.jpg", "dieZero.jpg", "dieZero.jpg"))
  scores <- reactiveValues(d1 = data.frame(
    Types = c("Ones", "Twos", "Threes", "Fours", "Fives", "Sixes", "Bonus", "3 of a kind", "4 of a kind", "Full House", "Small Straight", "Large Straight", "Yahtzee", "Chance"),
    Scores = NA
            )
  )
  
  ## submit button
  observeEvent(input$submit, {
    chosen$x <- append(chosen$x, input$selectScore)
    updateSelectInput(session, "selectScore", choices = options[!(options %in% chosen$x)])
    
    ## bonus yahtzee
    if(yahtzee$counter == 1){
      z = c(0, 0, 0, 0, 0, 0)
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(max(z) == 5){
        yahtzee$counter = yahtzee$counter + 1
        scores$d1[15,1] = "Bonus Yahtzees"
        scores$d1[15,2] = as.integer(100)
        output$bonusYahtzee <- renderText({"You got a bonus Yahtzee! An additional 100 points has been added to your score!"})
      }
      else{}
    }
    else if(yahtzee$counter >= 2){
      z = c(0, 0, 0, 0, 0, 0)
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(max(z) == 5){
        yahtzee$counter = yahtzee$counter + 1
        scores$d1[15,2] = scores$d1[15,2] + as.integer(100)
        output$bonusYahtzee <- renderText({"You got a bonus Yahtzee! An additional 100 points has been added to your score!"})
      }
      else{}
    }
    else{}
    
    ## input selections
    
    if(input$selectScore == "Ones")
      scores$d1[1,2] = as.integer(temp$score)
    else if(input$selectScore == "Twos")
      scores$d1[2,2] = as.integer(temp$score)
    else if(input$selectScore == "Threes")
      scores$d1[3,2] = as.integer(temp$score)
    else if(input$selectScore == "Fours")
      scores$d1[4,2] = as.integer(temp$score)
    else if(input$selectScore == "Fives")
      scores$d1[5,2] = as.integer(temp$score)
    else if(input$selectScore == "Sixes")
      scores$d1[6,2] = as.integer(temp$score)
    else if(input$selectScore == "3 of a kind")
      scores$d1[8,2] = as.integer(temp$score)
    else if(input$selectScore == "4 of a kind")
      scores$d1[9,2] = as.integer(temp$score)
    else if(input$selectScore == "Full House")
      scores$d1[10,2] = as.integer(temp$score)
    else if(input$selectScore == "Small Straight")
      scores$d1[11,2] = as.integer(temp$score)
    else if(input$selectScore == "Large Straight")
      scores$d1[12,2] = as.integer(temp$score)
    else if(input$selectScore == "Yahtzee"){
      scores$d1[13,2] = as.integer(temp$score)
      yahtzee$counter = yahtzee$counter + 1
    }
    else if(input$selectScore == "Chance")
      scores$d1[14,2] = as.integer(temp$score)
    
    ## cleanup/resetting
    dicevals$dice_vals = c(0, 0, 0, 0, 0)
    for(i in 1:5)
      imgvals$img_vals[i] = imageconvert(dicevals$dice_vals[i])
    times$rolltimes = 0
    timekeep$turns = timekeep$turns + 1
    output$noMore <- renderText({""})
    output$message <- renderText({""})
    updateCheckboxInput(session, "die1check", value = 0)
    updateCheckboxInput(session, "die2check", value = 0)
    updateCheckboxInput(session, "die3check", value = 0)
    updateCheckboxInput(session, "die4check", value = 0)
    updateCheckboxInput(session, "die5check", value = 0)
    
    ## top bonus code
    bonuscheck <- 0
    bonuscheck = sum(scores$d1[1:6,2], na.rm = TRUE)
    if(bonuscheck >= 63)
      scores$d1[7,2] = as.integer(35)
    else
      bonuscheck = 0
    
    ## timekeeping
    if(timekeep$turns == 13){
      output$finalturn <- renderText({paste0("Your game is over! You scored ", sum(scores$d1[1:15,2], na.rm = TRUE), " points!")})
    }
  })
  
  
  observeEvent(input$roll, {
    if(times$rolltimes == 0){
      output$bonusYahtzee <- renderText({""})
      
      ## get dice
      dicevals$dice_vals = sample(1:6, 5, replace = T)
      for(i in 1:5){
        imgvals$img_vals[i] = imageconvert(dicevals$dice_vals[i])
      }
      
      ## cleanup
      output$message <- renderText({""})
      updateSelectInput(session, "selectScore", choices = options[!(options %in% chosen$x)])
      times$rolltimes = times$rolltimes + 1
    }
    
    else if(times$rolltimes == 1){

      ## roll unselected dice
      if(input$die1check == FALSE){
        dicevals$dice_vals[1] = sample(1:6, 1)
        imgvals$img_vals[1] = imageconvert(dicevals$dice_vals[1])
      }
      if(input$die2check == FALSE){
        dicevals$dice_vals[2] = sample(1:6, 1)
        imgvals$img_vals[2] = imageconvert(dicevals$dice_vals[2])
      }
      if(input$die3check == FALSE){
        dicevals$dice_vals[3] = sample(1:6, 1)
        imgvals$img_vals[3] = imageconvert(dicevals$dice_vals[3])
      }
      if(input$die4check == FALSE){
        dicevals$dice_vals[4] = sample(1:6, 1)
        imgvals$img_vals[4] = imageconvert(dicevals$dice_vals[4])
      }
      if(input$die5check == FALSE){
        dicevals$dice_vals[5] = sample(1:6, 1)
        imgvals$img_vals[5] = imageconvert(dicevals$dice_vals[5])
      }
      
      ## cleanup
      output$message <- renderText({""})
      updateSelectInput(session, "selectScore", choices = options[!(options %in% chosen$x)])
      times$rolltimes = times$rolltimes + 1
    }
    
    else if(times$rolltimes == 2){
      
      ## roll unselected dice
      if(input$die1check == FALSE){
        dicevals$dice_vals[1] = sample(1:6, 1)
        imgvals$img_vals[1] = imageconvert(dicevals$dice_vals[1])
      }
      if(input$die2check == FALSE){
        dicevals$dice_vals[2] = sample(1:6, 1)
        imgvals$img_vals[2] = imageconvert(dicevals$dice_vals[2])
      }
      if(input$die3check == FALSE){
        dicevals$dice_vals[3] = sample(1:6, 1)
        imgvals$img_vals[3] = imageconvert(dicevals$dice_vals[3])
      }
      if(input$die4check == FALSE){
        dicevals$dice_vals[4] = sample(1:6, 1)
        imgvals$img_vals[4] = imageconvert(dicevals$dice_vals[4])
      }
      if(input$die5check == FALSE){
        dicevals$dice_vals[5] = sample(1:6, 1)
        imgvals$img_vals[5] = imageconvert(dicevals$dice_vals[5])
      }
      
      ## cleanup
      output$message <- renderText({""})
      updateSelectInput(session, "selectScore", choices = options[!(options %in% chosen$x)])
      times$rolltimes = times$rolltimes + 1
    }
    
    else if(times$rolltimes == 3){
      output$noMore <- renderText({"You have no more rolls!"})
    }
    else{}
    
  })
  
  ## ALL OF THE SCORE OPTIONS
  
  observeEvent(input$selectScore, {
    z = 0
    temp$score = z
    if(input$selectScore == ""){}
      
    else if(input$selectScore == "Ones"){
      z = 0
      temp$score = z
      for(i in 1:5){
        if(dicevals$dice_vals[i] == 1){
          z = z + 1
          temp$score = z
        }
      }
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Ones\" score?")})
    }
    
    else if(input$selectScore == "Twos"){
      z = 0
      temp$score = z
      for(i in 1:5){
        if(dicevals$dice_vals[i] == 2){
          z = z + 2
          temp$score = z
        }
      }
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Twos\" score?")})
    }
    
    else if(input$selectScore == "Threes"){
      z = 0
      temp$score = z
      for(i in 1:5){
        if(dicevals$dice_vals[i] == 3){
          z = z + 3
          temp$score = z
        }
      }
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Threes\" score?")})
    }
    
    else if(input$selectScore == "Fours"){
      z = 0
      temp$score = z
      for(i in 1:5){
        if(dicevals$dice_vals[i] == 4){
          z = z + 4
          temp$score = z
        }
      }
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Fours\" score?")})
    }
    
    else if(input$selectScore == "Fives"){
      z = 0
      temp$score = z
      for(i in 1:5){
        if(dicevals$dice_vals[i] == 5){
          z = z + 5
          temp$score = z
        }
      }
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Fives\" score?")})
    }
    
    else if(input$selectScore == "Sixes"){
      z = 0
      temp$score = z
      for(i in 1:5){
        if(dicevals$dice_vals[i] == 6){
          z = z + 6
          temp$score = z
        }
      }
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Sixes\" score?")})
    }
    
    else if(input$selectScore == "3 of a kind"){
      z = c(0, 0, 0, 0, 0, 0)
      temp$score = 0
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(max(z) >= 3){
        temp$score = sum(dicevals$dice_vals)
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"3 of a kind\" score?")})
      }
      else{
        temp$score = 0
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"3 of a kind\" score?")})
      }
    }
    
    else if(input$selectScore == "4 of a kind"){
      z = c(0, 0, 0, 0, 0, 0)
      temp$score = 0
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(max(z) >= 4){
        temp$score = sum(dicevals$dice_vals)
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"4 of a kind\" score?")})
      }
      else{
        temp$score = 0
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"4 of a kind\" score?")})
      }
    }
    
    else if(input$selectScore == "Full House"){
      z = c(0, 0, 0, 0, 0, 0)
      temp$score = 0
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      z = sort(z)
      if(z[6] == 3){
        if(z[5] == 2){
          temp$score = 25
          output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Full House\" score?")})
        }
        else{
          temp$score = 0
          output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Full House\" score?")})
        }
      }
      else{
        temp$score = 0
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Full House\" score?")})
      }
    }
    
    else if(input$selectScore == "Small Straight"){
      z = c(0, 0, 0, 0, 0, 0)
      temp$score = 0
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(z[1] >= 1 && z[2]>=1 && z[3]>=1 && z[4]>=1){
        temp$score = 30
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Small Straight\" score?")})
      }
      else if(z[2] >= 1 && z[3]>=1 && z[4]>=1 && z[5]>=1){
        temp$score = 30
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Small Straight\" score?")})
      }
      else if(z[3] >= 1 && z[4]>=1 && z[5]>=1 && z[6]>=1){
        temp$score = 30
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Small Straight\" score?")})
      }
      else{
        temp$score = 0
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Small Straight\" score?")})
      }
    }
    
    else if(input$selectScore == "Large Straight"){
      z = c(0, 0, 0, 0, 0, 0)
      temp$score = 0
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(z[1] == 1 && z[2] == 1 && z[3] == 1 && z[4] == 1 && z[5] == 1){
        temp$score = 40
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Large Straight\" score?")})
      }
      else if(z[6] == 1 && z[2] == 1 && z[3] == 1 && z[4] == 1 && z[5] == 1){
        temp$score = 40
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Large Straight\" score?")})
      }
      else{
        temp$score = 0
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Large Straight\" score?")})
      }
    }
    
    else if(input$selectScore == "Yahtzee"){
      z = c(0, 0, 0, 0, 0, 0)
      temp$score = 0
      for(i in 1:6){
        for(j in 1:5){
          if(dicevals$dice_vals[j] == i)
            z[i] = z[i] + 1
        }
      }
      if(max(z) == 5){
        temp$score = 50
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Yahtzee\" score?")})
      }
      else{
        temp$score = 0
        output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Yahtzee\" score?")})
      }
    }
    
    else if(input$selectScore == "Chance"){
      temp$score = sum(dicevals$dice_vals)
      output$message <- renderText({paste0("Would you like to submit ", temp$score, " as your \"Chance\" score?")})
    }
  })
  
  ## scorecard
  output$scorecard <- renderTable(width = "500", height = "1000", {
    scores$d1
  })
  
  ## dice photos
  
  output$die1 <- renderImage({list(src = imgvals$img_vals[1], height = "175", width = "175")  }, deleteFile = FALSE)
  output$die2 <- renderImage({list(src = imgvals$img_vals[2], height = "175", width = "175")  }, deleteFile = FALSE)
  output$die3 <- renderImage({list(src = imgvals$img_vals[3], height = "175", width = "175")  }, deleteFile = FALSE)
  output$die4 <- renderImage({list(src = imgvals$img_vals[4], height = "175", width = "175")  }, deleteFile = FALSE)
  output$die5 <- renderImage({list(src = imgvals$img_vals[5], height = "175", width = "175")  }, deleteFile = FALSE)
}



shinyApp(ui = ui, server = server)
