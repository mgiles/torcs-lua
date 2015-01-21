function drive (car)
  return { gear = getGear(car), accelCmd = 0.3 }
end

function getGear (car)
  local gear = car.gear
  local max = car.gearNb

  if gear <= 0 then
    return 1
  elseif gear < max and car.enginerpm >= car.enginerpmRedLine then
    return gear + 1
  else
    return gear
  end
end
