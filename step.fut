import "world"
import "alchemy"

-- Position in a Margolus neighborhood; ranges from 0-3.
type marg_pos = i32

-- A Margolus neighborhood.  We will just call these 'hood's, because
-- it is more gangsta.
type hood 't = (t,t,t,t)

-- The following two functions should be used for all hood
-- interaction.  Never just pattern patch directly on the value!
-- Pretend it is an abstract type.
let hoodQuadrants 't ((ul,ur,dl,dr): hood t): (t, t, t, t) =
  (ul,ur,dl,dr)

let hoodFromQuadrants 't (ul: t) (ur: t) (dl: t) (dr: t): hood t =
  (ul,ur,dl,dr)

-- Return the requested quadrant from the given hood.
let hoodQuadrant 't (h: hood t) (i: marg_pos): t =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if      i == 0 then ul0
  else if i == 1 then ur0
  else if i == 2 then dl0
  else                dr0

let indexToHood (offset: i32) (i: i32): (i32, i32) =
  if offset == 0 then (i / 2, i % 2)
  else ((i+1) / 2, (i+1) % 2)

-- Given a hood array at offset -1 or 0, return the element at index
-- (x,y).  Out-of-bounds returns the blank value.
let worldIndex [w][h] 't (blank: t) (offset: i32) (elems: [w][h](hood t)) ((x,y): (i32,i32)): t =
  -- First, figure out which hood (x,y) is in.
  let (hx,ix) = indexToHood offset x
  let (hy,iy) = indexToHood offset y

  -- Then read if we are in-bounds.
  in if hx < 0 || hx >= w || hy < 0 || hy >= h
     then blank
     else hoodQuadrant (unsafe elems[hx,hy]) (ix+iy*2)

-- From http://stackoverflow.com/a/12996028
let hash(x: i32): i32 =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) in
  x

-- An array with a "random" number for every hood.
let hoodRandoms ((w,h): (i32,i32)) ((lower,upper): (i32,i32)) (gen: i32): [w][h]i32 =
  map (\i -> (hash (gen ^ i*4)) % (upper-lower+1) + lower) (iota (w*h))
  |> unflatten w h

type phood = hood particle

-- Age every cell within a hood.  We use our (single) random number to
-- generate four new random numbers,which are then used for the aging.
let ageHood (seed: i32) (h: phood): phood =
  let (ul, ur, dl, dr) = hoodQuadrants h in
  hoodFromQuadrants (age (hash (seed^0) % 10000) ul)
                    (age (hash (seed^1) % 10000) ur)
                    (age (hash (seed^2) % 10000) dl)
                    (age (hash (seed^3) % 10000) dr)

-- Apply alchemy within a hood.
let alchemy (r: i32) (h: phood): phood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  if ul0 == ur0 && ur0 == dl0 && dl0 == dr0
  then h
  else -- Apply interaction among the components
  let (ul1, ur1, dr1, dl1) =
    loop (a, b, c, d) = (ul0, ur0, dr0, dl0) for _i < 4 do
      let (a', b') = applyAlchemy r a b
      in (b', c, d, a')
  in hoodFromQuadrants ul1 ur1 dl1 dr1

let checkIfDrop (above: particle) (below: particle): (particle, particle) =
  if isWall above || isWall below || weight below >= weight above
  then (above, below)
  else (below, above)

let applyConduction (x: particle) (y: particle): (particle, particle) =
  let diff = x.temp - y.temp in
  let c = f32.min (conductivity x) (conductivity y) * 10
  let delta = f32.max (-c) (f32.min c diff)
  in ({element=x.element, temp=x.temp - delta},
      {element=y.element, temp=y.temp + delta})

-- Diffuse heat within a hood.
let conduction (h: phood): phood =
  let (ul0, ur0, dl0, dr0) = hoodQuadrants h in
  let (ul1, ur1, dr1, dl1) =
    loop (a, b, c, d) = (ul0, ur0, dr0, dl0) for _i < 4 do
      let (a', b') = applyConduction a b
      in (b', c, d, a')
  in hoodFromQuadrants ul1 ur1 dl1 dr1

-- Apply gravity within a hood.
let gravity (h: phood): phood =
  let (ul, ur, dl, dr) = hoodQuadrants h

  let (ul, ur, dl, dr) =
    -- First check for fluid flow.
    if ((isFluid dl && dr.element == nothing) || (isFluid dr && dl.element == nothing)) &&
       isFluid ul && isFluid ur
    then (ul, ur, dr, dl)
    else if isFluid ul && weight ur < weight ul && dl.element != nothing && dr.element != nothing && !(isWall dl) && !(isWall dr)
    then (ur, ul, dl, dr)
    else if isFluid ur && weight ul < weight ur && dl.element != nothing && dr.element != nothing && !(isWall dl) && !(isWall dr)
    then (ur, ul, dr, dl)
    else if isFluid dl && weight ul < weight dl && weight ur < weight dl && weight dr < weight dl
    then (ul, ur, dr, dl)
    else if isFluid dr && weight ul < weight dr && weight ur < weight dr && weight dl < weight dr
    then (ul, ur, dr, dl)

    -- No fluid flow?  Let gravity do its work.
    else let (ul, dl) = checkIfDrop ul dl
         let (ur, dr) = checkIfDrop ur dr
         let (ul, dr) = checkIfDrop ul dr
         let (ur, dl) = checkIfDrop ur dl
         in (ul, ur, dl, dr)

  in hoodFromQuadrants ul ur dl dr

-- Compute interactions and aging for every hood, returning a new
-- array of hoods.
let one_step [w][h] (gen: i32) (hoods: [w][h]phood): [w][h]phood =
  let randomish = hoodRandoms (w,h) (0,10000) gen
  let envs = map2 (\randomish_r hoods_r -> map2 alchemy randomish_r hoods_r)
                  randomish hoods
  in map2 (map2 ageHood) randomish (map (map (gravity >-> conduction)) envs)
