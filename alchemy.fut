import "world"

-- The random parameter 'r' is used to decrease the frequency of some interactions.
let applyAlchemy (r: i32) (xp: particle) (yp: particle) : (particle, particle) =
  let {element=x, temp=xtemp} = xp
  let {element=y, temp=ytemp} = yp
  let px x' = {element=x', temp=xtemp}
  let py y' = {element=y', temp=ytemp}
  let p (x', y') = (px x', py y')
  in
  -- water + salt = salt_water + nothing
  if      x == water && y == salt
  then p (salt_water, nothing)
  else if x == salt && y == water
  then p (nothing, salt_water)

  -- cold steam + anything = water + anything
  else if x == steam_water && xtemp <= 363
  then p (water, y)
  else if y == steam_water && ytemp <= 363
  then p (x, water)

  -- hot water + anything = steam + anything
  else if x == water && xtemp >= 383
  then p (steam_water, y)
  else if y == water && ytemp >= 383
  then p (x, steam_water)

  -- hot water + nothing = steam + salt
  else if x == salt_water && xtemp >= 373 && x == nothing
  then p (steam_water, salt)
  else if x == nothing && y == salt_water && ytemp >= 373
  then p (salt, steam_water)

  -- oil + fire = new fire + new fire
  else if x == oil && isFire yp
  then p (fire, fire)
  else if isFire xp && y == oil
  then p (fire, fire)

  -- torch/napalm + nothing = torch/napalm + fire
  else if x == nothing && y == torch
  then ({element=fire, temp=initialTemp fire}, yp)
  else if x == torch && y == nothing
  then (xp, {element=fire, temp=initialTemp fire})

  -- spout + nothing = spout + water
  else if x == nothing && y == spout
  then p (water, spout)
  else if x == spout && y == nothing
  then p (spout, water)

  -- fire + plant = new fire + sand OR new fire + new fire
  else if isFire xp && y == plant
  then if r < 2000 then p (fire, sand) else p (fire, fire)
  else if x == plant && isFire yp
  then if r < 2000 then p (sand, fire) else p (fire, fire)

  -- water + plant = plant + plant
  else if x == water && y == plant
  then p (plant, plant)
  else if x == plant && y == water
  then p (plant, plant)

  else (xp, yp)
