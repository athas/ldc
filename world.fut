type element = u8
type weight = i32

type particle = { element: element
                , temp: f32 -- Kelvin.
                }

let nothing: element = 0
let steam_water: element = 1
let oil: element = 6
let water: element = 7
let salt_water: element = 8
let sand: element = 9
let salt: element = 10
let stone: element = 11
let fire: element = 12
let fire_end: element = 22
let torch: element = 23
let plant: element = 24
let spout: element = 25
let metal: element = 26
let wall: element = 29

let room_temp: f32 = 293
let empty: particle = { element = nothing, temp = room_temp }

let elems: []element = [ nothing
                       , steam_water
                       , oil
                       , water
                       , salt_water
                       , sand
                       , salt
                       , stone
                       , fire
                       , fire_end
                       , torch
                       , plant
                       , spout
                       , metal ]

let num_elems: i32 = length elems

let isWall ({element=x, temp=_}: particle): bool =
  x == torch || x == plant || x == spout || x == metal || x == wall

let isFire ({element=x, temp=_}: particle): bool =
  x >= fire && x <= fire_end

let isFluid ({element=x, temp=_}: particle): bool =
  x == steam_water || x == oil ||
  x == water || x == salt_water

let weight ({element=x, temp}: particle): weight =
  if x == nothing then 2
  else if x == steam_water then 0
  else if x == sand then i32.u8 salt
  else if isFire {element=x, temp} then 0
  else i32.u8 x

let initialTemp (x: element): f32 =
  if x == fire then 1100 else room_temp

let conductivity (p: particle): f32 =
  if p.element == metal || isFire p then 10
  else if p.element == water || p.element == steam_water then 0.2
  else if p.element == nothing then 0.001
  else 0.05

let age (r: i32) (p: particle): particle =
  let {element=x, temp} = p in
  if x == fire_end then {element=nothing, temp}
  else if isFire p then if r < 5000
                        then {element=x + 1, temp}
                        else p
  else p
