let say_hello () = print_endline "Hello, World!"

type colour = 
 | Red 
 | Green 
 | Blue 
 | Yellow
 | Mix of color * color

 let test_match = 
  let a = Mix (Red, Green) in
  let b = Mix (Red, Blue) in 
  print_endline "Hello, Match!"
