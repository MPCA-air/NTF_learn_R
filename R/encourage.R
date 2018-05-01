# Your very own encouragement kitty

encourage <- function(cat = FALSE) {
  
  #install.packages("cowsay")
  library(cowsay)
  
  quotes <- c("You are officially awesome at this!",
              "Keep going!",
              "All that hard work is paying off!",
              "You are quite good my friend!",
              "You are on a roll.",
              "Excellent!",
              "Keep the streak going.",
              "You're unstoppable!",
              "Meow meow!",
              "You're making your teachers proud.",
              "You have serious R skills.",
              "That was nicely done.",
              "You are on your way.",
              "Great job!",
              "My pet chicken says you're EGG-CELLENT!",
              "Keep being amazing!",
              "You're on fire!",
              "That was purrfect!",
              "Wow! You are too good. Let's paws for a moment.",        
              "Great job! Fur real!",
              "You sure aren't kitten around! Great work!",
              "You didn't let that one give you paws! Excellent work!",
              "Feline fine after that exercise! Great job!",
              "You are a CATalyst to success!",
              "Breaking Meows! You rock!",
              "Not too shabby for a tabby!",
              "Way to dig your claws in!",
              "You got the whole kitten-kaboodle."
              )
  
  # Get random encouragement
  quote <- sample(quotes, 1)

  # Print to screen
  if (cat) return(say(paste0("Meow. ", quote), "behindcat"))
  
  return(quote)
  
}

