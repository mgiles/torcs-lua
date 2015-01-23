function drive (car)
  return { gear = getGear(car), accelCmd = 0.3, steer = getSteer(car) }
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

function getSteer (car)
  local angle = torcs.RtTrackSideTgAngleL(car.pub.trkPos) - yaw(car)
  angle = norm_pi(angle)
  angle = angle - (car.pub.trkPos.toMiddle / car.pub.trkPos.seg.width)

  steerLock = car.info.steerLock

  return angle / steerLock
end

function yaw (car)
  return car.pub.DynGC.pos.az
end

function norm_pi (angle)
  while angle > math.pi do
    angle = angle - 2*math.pi
  end

  while angle < (-math.pi) do
    angle = angle + 2*math.pi
  end

  return angle
end
