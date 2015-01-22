function drive (car)
  return { gear = getGear(car), accelCmd = 0.3 }
end

function getGear (car)
  local gear = car.priv.gear
  local max = car.priv.gearNb

  if gear <= 0 then
    return 1
  elseif gear < max and car.priv.enginerpm >= car.priv.enginerpmRedLine then
    return gear + 1
  else
    return gear
  end
end
