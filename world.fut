type element = u8
type weight = u8
type weight_env = u8

type particle = { element: element
                , temp: f32 -- Kelvin.
                }

let nothing: element = 0u8
let steam_water: element = 1u8
let oil: element = 6u8
let water: element = 7u8
let salt_water: element = 8u8
let sand: element = 9u8
let salt: element = 10u8
let stone: element = 11u8
let fire: element = 12u8
let fire_end: element = 22u8
let torch: element = 23u8
let plant: element = 24u8
let spout: element = 25u8
let metal: element = 26u8
let turnip: element = 28u8
let wall: element = 29u8
let napalm: element = 30u8

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
                       , metal
                       , napalm
                       , turnip ]

let num_elems: i32 = length elems

let isWall ({element=x, temp=_}: particle): bool =
  x == torch || x == plant || x == spout || x == metal || x == wall

let isFire ({element=x, temp=_}: particle): bool =
  x >= fire && x <= fire_end

let isFluid ({element=x, temp=_}: particle): bool =
  x == steam_water || x == oil ||
  x == water || x == salt_water ||
  x == napalm

let weight ({element=x, temp}: particle): weight =
  if x == nothing then 2u8
  else if x == steam_water then 0u8
  else if x == sand then salt
  else if x == napalm then water - 1u8
  else if isFire {element=x, temp} then 0u8
  else x

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
  else if isFire p then if r < 5000 then {element=x + 1u8, temp} else p
  else if x == turnip then unsafe {element=elems[r%num_elems], temp}
  else p
