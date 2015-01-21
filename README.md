**Work in progress**

This is a sample driver for TORCS that allows the driver logic to be implemented in Lua instead of C++. The bindings are
currently very limited, but will continue to be filled in.

## Compatibility

Tested on Debian with TORCS 1.3.6 and Lua 5.1

## Basic setup instructions

1. Download, build, and install TORCS (instructions [here](http://torcs.sourceforge.net/index.php?name=Sections&op=viewarticle&artid=3))
2. Clone this repository into the TORCS drivers directory (ex. into `torcs-1.3.6/src/drivers`)
3. `make && make install`
4. Now in TORCS when you're configuring a race there will be a `lua_sample` driver available.

For building TORCS bots in general, you need to set the following environment variables.

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
export TORCS_BASE=/path/to/torcs/base
export MAKE_DEFAULT=$TORCS_BASE/Make-default.mk
```

You can find more instructions for getting TORCS set up in berniw's 
[TORCS Robot Tutorial](http://www.berniw.org/tutorials/robot/tutorial.html).
The robot tutorial is a bit out of date, so you may need to deviate from it in places. Try the
[torcs-users](http://sourceforge.net/p/torcs/mailman/torcs-users/) mailing list if you run into problems.

## Further setup

In the `Makefile`, make sure the `LIBS` and `COMPILFLAGS` variables use the correct paths for your installation of Lua.
The existing values are for the default install locations for Lua 5.1 on Debian.

The path to the main Lua script (`drive.lua` by default) is set in `lua_bridge.cpp` in the `initLua` function. It relies
on the same `TORCS_BASE` environment variable described above. If you rename or move the Lua script, you'll
need to make sure this function can still find it.

## Bindings

There is currently a limited set of bindings in `dispatch.cpp`. The `carFields` table defines the names and functions for
field accessors on the `tCarElt` structure (TORCS passes the game state to the robot in this struct). In `drive.lua`, the
main `drive` function is passed a variable that holds this state. The dispatch code is set up so that you can use the
names in `carFields` as fields on the state in Lua. See, for example, this code from `drive.lua`:

```lua
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
```

The call `car.gear` will end up calling the `tl_gear` function in `dispatch.cpp` and will return the current gear.

The `drive` function must return a Lua table with the vehicle control fields in it. The available fields are `steer`,
`accelCmd`, `brakeCmd`, `clutchCmd`, `gear`, `raceCmd`, and `lightCmd`. All of the fields have a default value of 0 if
you don't set them. You can see how these fields are passed back to TORCS in the `tl_drive` function in `lua_bridge.cpp`.
