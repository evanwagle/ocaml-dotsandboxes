Install Instructions for 3110 Final Project: Dots and Boxes Game: 

Necessary Packages: 
- OUnit2 
- ANSITerminal 
- Graphics 

To execute (2 ways to play):

Play game in GUI:
- To play simulation of AI vs AI: 
  - At the bottom of draw_board in gui.ml, change the respective modes of the bots (either "Easy", "Medium", "Hard"). Ex:   
  ```
  if mode = "Simulation" then
    loop_simulation default_board "Medium" "Hard" bot1
  ```
  - Change the string input in open_board so it is "Simulation". Ex:
  ```
  draw_board board_dimensions window_dimensions counter_dimensions "Simulation"
  ```

- To play Multiplayer: 
  - At the bottom of draw_board in gui.ml, change the respective modes to "Mult". Ex:   
  ```
  else player_input () default_board player1 "Mult"
  ```
  - Change the string input in open_board so it is "Mult". Ex:
  ```
  draw_board board_dimensions window_dimensions counter_dimensions "Mult"
  ```

- To play against AI: 
  - At the bottom of gui.ml change the string input in draw_board so it is the AI difficulty you want. Ex: 
  ```
  else player_input () default_board player1 "Easy"
  ```
  - Change the string input in open_board so it is the difficulty. Ex:
  ```
  draw_board board_dimensions window_dimensions counter_dimensions "Easy"
  ```
- Run make clean & make build
- Go into utop
- Type #use "gui.ml" into console
- GUI will then appear in a window on your desktop
- Press q on keyboard to quit out of GUI

Play game in terminal:
- Run make clean & make build
- Type "rm main.byte"
- Run make play
- Type the mode you want to play


