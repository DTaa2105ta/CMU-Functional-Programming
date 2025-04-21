
datatype instruction = Right
                     | Step of int

type itinerary = instruction list

datatype orientation = North | East | South | West

type position = int * int * orientation


fun turnRight (x: int, y: int, orient: orientation) : position= 
  case orient 
    of North => (x, y, East)
     | East  => (x, y, South)
     | South => (x, y, West)
     | West  => (x, y, North);

fun move (move: int, (x: int, y: int, orient: orientation) : position) : position = 
  case orient 
    of North => (x, y + move, North)
     | East  => (x + move, y, East)
     | South => (x, y - move, South)
     | West  => (x - move, y, West);

fun getPosition (l: itinerary, pose: position) : position = 
  case l 
    of [] => pose
     | x::xs => (case x of Right => getPosition (xs, turnRight pose)
                         | Step n => getPosition (xs, move(n, pose)));
(* Test cases for getPosition *)
val test1 = getPosition ([Right, Step 2, Right, Step 3], (0, 0, North)) = (2, ~3, South);
(* Expected: true *)
val test2 = getPosition ([Right, Step 1, Right, Step 1], (0, 0, North)) = (1, 1, West);
(* Expected: fasle *)          
val test3 = (5, 1, East) = getPosition([Step 2, Right, Step 2, Right, Right, Right, Step 2, Right, Step 2, Right, Step 3, Right, Right, Right, Step 1], (0, 0, North))
(* Expected: true *)


fun reverse ([] : itinerary) : itinerary = [] 
  | reverse (Step n :: xs) = reverse xs @ [Step n]
  | reverse (Right :: (Right :: (Right :: xs))) = reverse xs @ [Right]
  | reverse (Right :: (Right :: xs)) = reverse xs @ [Right, Right]
  | reverse (Right :: xs) = reverse xs @ [Right, Right, Right];

fun goBack (l : itinerary) : itinerary= [Right, Right] @ reverse l @ [Right, Right];
(* goBack Property *)
(* getPosition(l @ goBack(l) @ [right,right],p) = turnRight(turnRight(p)) *)
(* Test cases for goBack Property *)
val test4 = getPosition([Right, Step 2, Right, Step 3] @ goBack([Right, Step 2, Right, Step 3]) @ [Right, Right], (0, 0, North)) = turnRight(turnRight(0, 0, North));
(* Expected: true *)
val l5 : itinerary = [Step 10, Right, Step 5, Right, Step 10, Right, Step 5];
val test5 = getPosition(l5 @ goBack(l5) @ [Right, Right], (1, 2, East)) = turnRight(turnRight(1, 2, East));
(* Expected: true *)