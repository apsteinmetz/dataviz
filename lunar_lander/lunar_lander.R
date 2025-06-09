# Lunar Lander Game - R Version
# Based on the classic PDP-8 FOCAL game

altitude_plot <- function(altitude = 1000, velocity =50, fuel= 150) {
  altitude <- as.integer(altitude / 10)
  moon <- "🌕 "
  up <- "▶️" # Right Arrow
  down <- "◀️" # Left Arrow
  lander <- "🚀" # Rocket
  warning <- "⚠️" # Warning
  panic <- "😮"
  # double right arrow
  up2 <- "⏩ "
  down2 <- "⏪ "

  if (fuel < start_fuel / 10) lander <- warning
  if (velocity < 0) lander <- warning
  if (fuel < (start_fuel / 20) && velocity > 5 && altitude < 100) lander <- panic
  ship_status <- switch(
      as.character(cut(
        velocity,
        breaks = c(-Inf, -5, 0, 5, Inf),
        labels = c("up2", "up", "down", "down2")
      )),
      "up2" = paste0(moon, strrep(".", altitude), lander, up2),
      "up" = paste0(moon, strrep(".", altitude), lander, up),
      "down" = paste0(moon, strrep(".", altitude), down, lander),
      "down2" = paste0(moon, strrep(".", altitude), down2, lander)
    )
  return(ship_status)
}

lunar_lander <- function() {
  cat("LUNAR LANDER\n")
  cat("============\n\n")
  cat("You are piloting a lunar module to the moon's surface.\n")
  cat("You must land safely by controlling your thrust.\n")
  cat("A soft landing requires a velocity of 5 ft/sec or less.\n\n")
  cat("<ESC> to quit\n\n")
  
  # Game loop
  repeat {
    # Initialize game variables
    altitude <- 1000      # Starting altitude in feet
    velocity <- 50        # Starting velocity in ft/sec (downward is positive)
    start_fuel <- 150  # Starting fuel in units
    fuel <- start_fuel          # Starting fuel in units
    time <- 0            # Mission time in seconds
    
    cat("MISSION STARTS:\n")
    cat(sprintf("Altitude: %d ft, Velocity: %d ft/sec, Fuel: %d units\n\n", 
                altitude, velocity, fuel))
    
    # Main game loop
    while (altitude > 0) {
      # Get thrust input from player
      cat(sprintf("Time: %d sec, Alt: %d ft, Vel: %.1f ft/sec, Fuel: %d\n %s", 
                  as.integer(time), as.integer(max(0, altitude)), velocity, as.integer(fuel),
                  altitude_plot(altitude, velocity, fuel)))
      
      # Input validation loop
      repeat {
        thrust_input <- readline("Thrust (0-50): ")
        if (tolower(thrust_input) == "q") {
          cat("\nQuitting the game. Goodbye!\n")
          break
        } else if (tolower(thrust_input) == "show_instructions()") {
          show_instructions()
          next
        }
        thrust <- suppressWarnings(as.numeric(thrust_input))
        
        if (is.na(thrust)) {
          cat("Please enter a number.\n")
          next
        }
        
        if (thrust < 0) {
          cat("Thrust cannot be negative.\n")
          next
        }
        
        if (thrust > 50) {
          cat("Maximum thrust is 50.\n")
          next
        }
        
        if (thrust > fuel) {
          cat(sprintf("Not enough fuel! You only have %d units.\n", fuel))
          next
        }
        
        break
      }
      
      # Physics simulation (1 second time step)
      fuel <- fuel - thrust
      
      # Calculate acceleration due to thrust and gravity
      # Moon gravity is about 1/6 of Earth's (5.3 ft/sec²)
      gravity <- 5.3
      thrust_accel <- thrust * 2  # Thrust acceleration factor
      
      # Update velocity (positive is downward)
      velocity <- velocity + gravity - thrust_accel
      
      # Update altitude
      altitude <- altitude - velocity
      
      # Update time
      time <- time + 1
      
      cat("\n")
      
      # Check if out of fuel
      if (fuel <= 0 && altitude > 0) {
        cat("*** OUT OF FUEL ***\n")
        cat("Falling under gravity alone...\n\n")
        
        # Continue simulation without thrust until landing
        while (altitude > 0) {
          velocity <- velocity + gravity
          altitude <- altitude - velocity
          time <- time + 1
          
          if (time %% 5 == 0) {  # Show status every 5 seconds
            cat(sprintf("Time: %d sec, Alt: %d ft, Vel: %.1f ft/sec\n", 
                        as.integer(time), as.integer(max(0, altitude)), velocity))
          }
        }
        break
      }
    }
    
    # Landing results
    cat("\n*** CONTACT WITH SURFACE ***\n")
    cat(sprintf("Landing velocity: %.1f ft/sec\n", velocity))
    
    # Determine landing outcome
    if (velocity <= 5) {
      cat("\n🎉 CONGRATULATIONS! 🎉\n")
      cat("PERFECT LANDING!\n")
      cat("The Eagle has landed safely.\n")
    } else if (velocity <= 15) {
      cat("\n⚠️  ROUGH LANDING\n")
      cat("You made it down, but the landing was rough.\n")
      cat("Some equipment may be damaged.\n")
    } else if (velocity <= 30) {
      cat("\n💥 CRASH LANDING\n")
      cat("You crashed! The lunar module is badly damaged.\n")
      cat("Mission abort - but at least you're alive.\n")
    } else {
      cat("\n💀 CATASTROPHIC CRASH\n")
      cat("You hit the surface way too hard!\n")
      cat("The lunar module is completely destroyed.\n")
      cat("Mission failure.\n")
    }
    
    cat(sprintf("\nMission statistics:\n"))
    cat(sprintf("Total mission time: %d seconds\n", time))
    cat(sprintf("Fuel remaining: %d units\n", max(0, fuel)))
    
    # Play again?
    cat("\n")
    play_again <- readline("Play again? (y/n): ")
    if (tolower(substr(play_again, 1, 1)) != "y") {
      break
    }
    cat("\n" %+% paste(rep("=", 50), collapse="") %+% "\n\n")
  }
  
  cat("\nThanks for playing Lunar Lander!\n")
}

# Helper function for string concatenation
`%+%` <- function(a, b) paste0(a, b)

# Instructions function
show_instructions <- function() {
  cat("LUNAR LANDER - INSTRUCTIONS\n")
  cat("==========================\n\n")
  cat("OBJECTIVE:\n")
  cat("Land your lunar module safely on the moon's surface.\n\n")
  cat("CONTROLS:\n")
  cat("- Enter thrust values from 0 to 50\n")
  cat("- Higher thrust slows your descent but uses more fuel\n")
  cat("- You start with 150 units of fuel\n\n")
  cat("PHYSICS:\n")
  cat("- Moon gravity pulls you down at 5.3 ft/sec²\n")
  cat("- Thrust pushes you up (against gravity)\n")
  cat("- Positive velocity = falling down\n")
  cat("- Negative velocity = moving up\n\n")
  cat("LANDING CRITERIA:\n")
  cat("- Perfect landing: ≤ 5 ft/sec\n")
  cat("- Rough landing: 6-15 ft/sec\n")
  cat("- Crash landing: 16-30 ft/sec\n")
  cat("- Catastrophic: > 30 ft/sec\n\n")
  cat("STRATEGY:\n")
  cat("- Don't use too much thrust early on\n")
  cat("- Save fuel for the final approach\n")
  cat("- Try to zero out your velocity just as you reach the surface\n\n")
}

# Start the game
cat("Welcome to Lunar Lander!\n")
cat("Type 'show_instructions()' for detailed instructions.\n")
cat("Type 'Q to Quit.\n")
cat("Type 'lunar_lander()' to start playing.\n\n")

# Run the game immediately
lunar_lander()

